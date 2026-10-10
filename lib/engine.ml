type t = {
  lex : Lexer.t;
  macros : (string, string) Hashtbl.t;
  builtins : (string, Builtins.builtin_fn) Hashtbl.t;
  diversions : (int, Buffer.t) Hashtbl.t;
  mutable current_div : int;
  sandbox_root : string;
  max_depth : int;
}

let create ?(sandbox_root = ".") () =
  let t = {
    lex = Lexer.create ();
    macros = Hashtbl.create 32;
    builtins = Hashtbl.create 32;
    diversions = Hashtbl.create 8;
    current_div = 0;
    sandbox_root;
    max_depth = 1024;
  } in
  Builtins.register_defaults (Hashtbl.add t.builtins);
  t

let define t name value =
  Hashtbl.replace t.macros name value

let has_macro_or_builtin t name =
  Hashtbl.mem t.builtins name || Hashtbl.mem t.macros name

let is_raw_builtin name =
  match name with
  | "h" | "html_escape" | "json_escape" | "sh_quote" -> true
  | _ -> false

let write_output t s =
  if t.current_div >= 0 then
    let buf =
      match Hashtbl.find_opt t.diversions t.current_div with
      | Some b -> b
      | None ->
          let b = Buffer.create 64 in
          Hashtbl.add t.diversions t.current_div b;
          b
    in
    Buffer.add_string buf s

let substitute_args body macro_name args =
  let s = ref body in
  let replace pat sub =
    s := Str.global_replace (Str.regexp_string pat) sub !s
  in
  replace "$0" macro_name;
  replace "$#" (string_of_int (List.length args));
  replace "$*" (String.concat "," args);
  replace "$@" (String.concat "," args);
  List.iteri (fun i arg ->
    replace ("$" ^ string_of_int (i + 1)) arg
  ) args;
  !s

let rec expand t input =
  Lexer.push_input t.lex input;
  let depth = ref 0 in
  let continue_loop = ref true in
  let result_err = ref None in

  while !continue_loop do
    match Lexer.next_token t.lex with
    | Lexer.TokenEOF -> continue_loop := false
    | Lexer.TokenQuoted s -> write_output t s
    | Lexer.TokenComment s -> write_output t s
    | Lexer.TokenOther c -> write_output t (String.make 1 c)
    | Lexer.TokenIdent name ->
        if not (has_macro_or_builtin t name) then
          write_output t name
        else begin
          match collect_args t name with
          | Error e ->
              result_err := Some e;
              continue_loop := false
          | Ok (args, _has_paren) ->
              incr depth;
              if !depth > t.max_depth then begin
                result_err := Some "maximum macro recursion depth exceeded";
                continue_loop := false
              end else if is_raw_builtin name then
                match eval_macro t name args with
                | Ok res -> write_output t res
                | Error e ->
                    result_err := Some e;
                    continue_loop := false
              else
                match eval_macro t name args with
                | Ok exp -> Lexer.push_input t.lex exp
                | Error e ->
                    result_err := Some e;
                    continue_loop := false
        end
  done;

  match !result_err with
  | Some e -> Error e
  | None ->
      let out0 =
        match Hashtbl.find_opt t.diversions 0 with
        | Some b -> Buffer.contents b
        | None -> ""
      in
      Ok out0

and collect_args t macro_name =
  match Lexer.read_char t.lex with
  | None -> Ok ([], false)
  | Some '(' ->
      let (lq, rq) = Lexer.get_quotes t.lex in
      let args = ref [] in
      let current = Buffer.create 16 in
      let paren_depth = ref 0 in
      let finished = ref false in
      let err_res = ref None in

      let process_arg raw =
        let s = String.trim raw in
        let lq_len = String.length lq in
        let rq_len = String.length rq in
        let s_len = String.length s in
        if s_len >= lq_len + rq_len &&
           String.sub s 0 lq_len = lq &&
           String.sub s (s_len - rq_len) rq_len = rq then
          Ok (String.sub s lq_len (s_len - lq_len - rq_len))
        else if s = "" then Ok ""
        else
          let sub_engine = create ~sandbox_root:t.sandbox_root () in
          Hashtbl.iter (Hashtbl.replace sub_engine.macros) t.macros;
          Hashtbl.iter (Hashtbl.replace sub_engine.builtins) t.builtins;
          Lexer.set_quotes sub_engine.lex lq rq;
          expand sub_engine s
      in

      while not !finished do
        if Lexer.match_prefix t.lex lq then begin
          let inner = Lexer.read_until_matching t.lex lq rq in
          Buffer.add_string current (lq ^ inner ^ rq)
        end else
          match Lexer.read_char t.lex with
          | None ->
              err_res := Some (Printf.sprintf "unexpected EOF reading macro arguments for %S" macro_name);
              finished := true
          | Some '(' ->
              incr paren_depth;
              Buffer.add_char current '('
          | Some ')' ->
              if !paren_depth = 0 then begin
                (match process_arg (Buffer.contents current) with
                 | Ok arg_str ->
                     if not (!args = [] && Buffer.length current = 0) then
                       args := arg_str :: !args
                 | Error e -> err_res := Some e);
                finished := true
              end else begin
                decr paren_depth;
                Buffer.add_char current ')'
              end
          | Some ',' ->
              if !paren_depth = 0 then begin
                (match process_arg (Buffer.contents current) with
                 | Ok arg_str -> args := arg_str :: !args
                 | Error e -> err_res := Some e);
                Buffer.clear current
              end else
                Buffer.add_char current ','
          | Some c ->
              Buffer.add_char current c
      done;

      (match !err_res with
       | Some e -> Error e
       | None -> Ok (List.rev !args, true))
  | Some other ->
      Lexer.unread_char t.lex other;
      Ok ([], false)

and eval_macro t name args =
  match name with
  | "define" ->
      (match args with
       | k :: v :: _ -> Hashtbl.replace t.macros k v; Ok ""
       | [k] -> Hashtbl.replace t.macros k ""; Ok ""
       | [] -> Ok "")
  | "undefine" ->
      (match args with
       | k :: _ -> Hashtbl.remove t.macros k; Ok ""
       | [] -> Ok "")
  | "ifdef" ->
      (match args with
       | k :: t_branch :: rest ->
           if has_macro_or_builtin t k then Ok t_branch
           else (match rest with f_branch :: _ -> Ok f_branch | [] -> Ok "")
       | _ -> Ok "")
  | "ifelse" ->
      let rec eval_ifelse = function
        | a :: b :: matched :: _ when a = b -> Ok matched
        | _ :: _ :: _ :: [default] -> Ok default
        | _ :: _ :: _ :: rest -> eval_ifelse rest
        | _ -> Ok ""
      in
      eval_ifelse args
  | "changequote" ->
      (match args with
       | [] | [""] -> Lexer.set_quotes t.lex "`" "'"; Ok ""
       | [l] -> Lexer.set_quotes t.lex l "'"; Ok ""
       | l :: r :: _ -> Lexer.set_quotes t.lex l r; Ok "")
  | "changecom" ->
      (match args with
       | [] | [""] -> Lexer.set_comments t.lex "" ""; Ok ""
       | [l] -> Lexer.set_comments t.lex l "\n"; Ok ""
       | l :: r :: _ -> Lexer.set_comments t.lex l r; Ok "")
  | "dnl" ->
      let continue_dnl = ref true in
      while !continue_dnl do
        match Lexer.read_char t.lex with
        | None | Some '\n' -> continue_dnl := false
        | Some _ -> ()
      done;
      Ok ""
  | "shift" ->
      (match args with
       | _ :: tl ->
           let (lq, rq) = Lexer.get_quotes t.lex in
           Ok (String.concat "," (List.map (fun s -> lq ^ s ^ rq) tl))
       | [] -> Ok "")
  | "defn" ->
      (match args with
       | k :: _ ->
           (match Hashtbl.find_opt t.macros k with
            | Some v ->
                let (lq, rq) = Lexer.get_quotes t.lex in
                Ok (lq ^ v ^ rq)
            | None -> Ok "")
       | [] -> Ok "")
  | _ ->
      match Hashtbl.find_opt t.builtins name with
      | Some fn ->
          let ctx : Builtins.context = {
            get_macro = Hashtbl.find_opt t.macros;
            set_macro = Hashtbl.replace t.macros;
            delete_macro = Hashtbl.remove t.macros;
            has_macro_or_builtin = has_macro_or_builtin t;
            get_lexer = t.lex;
            set_current_div = (fun d -> t.current_div <- d);
            get_diversion = (fun d ->
              match Hashtbl.find_opt t.diversions d with
              | Some b -> b
              | None ->
                  let b = Buffer.create 64 in
                  Hashtbl.add t.diversions d b;
                  b);
            remove_diversion = Hashtbl.remove t.diversions;
            get_all_diversion_keys = (fun () ->
              Hashtbl.fold (fun k _ acc -> k :: acc) t.diversions []);
            get_sandbox_root = (fun () -> t.sandbox_root);
          } in
          fn ctx args
      | None ->
          match Hashtbl.find_opt t.macros name with
          | Some body -> Ok (substitute_args body name args)
          | None -> Ok ""

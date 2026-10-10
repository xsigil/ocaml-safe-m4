type context = {
  get_macro : string -> string option;
  set_macro : string -> string -> unit;
  delete_macro : string -> unit;
  has_macro_or_builtin : string -> bool;
  get_lexer : Lexer.t;
  set_current_div : int -> unit;
  get_diversion : int -> Buffer.t;
  remove_diversion : int -> unit;
  get_all_diversion_keys : unit -> int list;
  get_sandbox_root : unit -> string;
}

type builtin_fn = context -> string list -> (string, string) result

let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&#34;"
    | '\'' -> Buffer.add_string buf "&#39;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let json_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let sh_quote s =
  "'" ^ Str.global_replace (Str.regexp_string "'") "'\\''" s ^ "'"

let expand_ranges s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    if !i + 2 < len && s.[!i + 1] = '-' && s.[!i] <= s.[!i + 2] then begin
      let start_c = Char.code s.[!i] in
      let end_c = Char.code s.[!i + 2] in
      for c = start_c to end_c do
        Buffer.add_char buf (Char.chr c)
      done;
      i := !i + 3
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let builtin_len _ctx args =
  match args with
  | [] -> Ok "0"
  | s :: _ -> Ok (string_of_int (String.length s))

let builtin_index _ctx args =
  match args with
  | s :: sub :: _ ->
      (try
        let idx = Str.search_forward (Str.regexp_string sub) s 0 in
        Ok (string_of_int idx)
       with Not_found -> Ok "-1")
  | _ -> Ok "-1"

let builtin_substr _ctx args =
  match args with
  | [] -> Ok ""
  | s :: rest ->
      let len_s = String.length s in
      let from_pos =
        match rest with
        | from_str :: _ when from_str <> "" ->
            (try int_of_string from_str with _ -> 0)
        | _ -> 0
      in
      if from_pos < 0 || from_pos >= len_s then Ok ""
      else
        match rest with
        | _ :: len_str :: _ when len_str <> "" ->
            let sub_len = (try int_of_string len_str with _ -> len_s - from_pos) in
            if sub_len <= 0 then Ok ""
            else
              let actual_len = min sub_len (len_s - from_pos) in
              Ok (String.sub s from_pos actual_len)
        | _ -> Ok (String.sub s from_pos (len_s - from_pos))

let builtin_translit _ctx args =
  match args with
  | [] -> Ok ""
  | [s] -> Ok s
  | s :: from_str :: rest ->
      let to_str = match rest with to_pat :: _ -> to_pat | [] -> "" in
      let from_exp = expand_ranges from_str in
      let to_exp = expand_ranges to_str in
      let tbl = Hashtbl.create 16 in
      let del_set = Hashtbl.create 16 in
      let from_len = String.length from_exp in
      let to_len = String.length to_exp in
      for i = 0 to from_len - 1 do
        let fc = from_exp.[i] in
        if i < to_len then Hashtbl.replace tbl fc to_exp.[i]
        else Hashtbl.replace del_set fc true
      done;
      let buf = Buffer.create (String.length s) in
      String.iter (fun c ->
        if not (Hashtbl.mem del_set c) then
          match Hashtbl.find_opt tbl c with
          | Some rep -> Buffer.add_char buf rep
          | None -> Buffer.add_char buf c
      ) s;
      Ok (Buffer.contents buf)

let builtin_incr _ctx = function
  | n_str :: _ ->
      (try Ok (string_of_int (int_of_string (String.trim n_str) + 1))
       with _ -> Error (Printf.sprintf "incr: non-numeric argument %S" n_str))
  | [] -> Error "incr: missing argument"

let builtin_decr _ctx = function
  | n_str :: _ ->
      (try Ok (string_of_int (int_of_string (String.trim n_str) - 1))
       with _ -> Error (Printf.sprintf "decr: non-numeric argument %S" n_str))
  | [] -> Error "decr: missing argument"

let eval_simple expr_str =
  let s = String.trim expr_str in
  try
    let lexbuf = Lexing.from_string s in
    let tokens = ref [] in
    let rec tokenize () =
      if lexbuf.Lexing.lex_curr_pos >= String.length s then ()
      else
        let c = s.[lexbuf.Lexing.lex_curr_pos] in
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos + 1;
        if c = ' ' || c = '\t' || c = '\n' then tokenize ()
        else if c >= '0' && c <= '9' then begin
          let start = lexbuf.Lexing.lex_curr_pos - 1 in
          while lexbuf.Lexing.lex_curr_pos < String.length s &&
                s.[lexbuf.Lexing.lex_curr_pos] >= '0' && s.[lexbuf.Lexing.lex_curr_pos] <= '9' do
            lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos + 1
          done;
          let num_str = String.sub s start (lexbuf.Lexing.lex_curr_pos - start) in
          tokens := `INT (int_of_string num_str) :: !tokens;
          tokenize ()
        end else begin
          let tok = match c with
            | '+' -> `ADD | '-' -> `SUB | '*' -> `MUL | '/' -> `DIV | '%' -> `MOD
            | '(' -> `LPAREN | ')' -> `RPAREN
            | '>' -> `GT | '<' -> `LT | '=' -> `EQ | '!' -> `NOT
            | _ -> failwith "invalid char"
          in
          tokens := tok :: !tokens;
          tokenize ()
        end
    in
    tokenize ();
    let tok_list = ref (List.rev !tokens) in
    let peek () = match !tok_list with [] -> `EOF | hd :: _ -> hd in
    let consume () = match !tok_list with [] -> () | _ :: tl -> tok_list := tl in

    let rec parse_factor () =
      match peek () with
      | `INT n -> consume (); n
      | `SUB -> consume (); -(parse_factor ())
      | `LPAREN ->
          consume ();
          let v = parse_expr () in
          if peek () = `RPAREN then consume () else failwith "mismatched paren";
          v
      | _ -> failwith "syntax error in factor"
    and parse_term () =
      let rec loop acc =
        match peek () with
        | `MUL -> consume (); loop (acc * parse_factor ())
        | `DIV -> consume (); let r = parse_factor () in loop (acc / r)
        | `MOD -> consume (); let r = parse_factor () in loop (acc mod r)
        | _ -> acc
      in
      loop (parse_factor ())
    and parse_add () =
      let rec loop acc =
        match peek () with
        | `ADD -> consume (); loop (acc + parse_term ())
        | `SUB -> consume (); loop (acc - parse_term ())
        | _ -> acc
      in
      loop (parse_term ())
    and parse_expr () =
      let lhs = parse_add () in
      match peek () with
      | `GT -> consume (); if lhs > parse_add () then 1 else 0
      | `LT -> consume (); if lhs < parse_add () then 1 else 0
      | `EQ -> consume (); if peek () = `EQ then consume (); if lhs = parse_add () then 1 else 0
      | _ -> lhs
    in
    Ok (parse_expr ())
  with e -> Error (Printexc.to_string e)

let builtin_eval _ctx args =
  match args with
  | [] -> Ok "0"
  | expr :: rest ->
      match eval_simple expr with
      | Ok v ->
          let radix =
            match rest with
            | r_str :: _ when r_str <> "" -> (try int_of_string (String.trim r_str) with _ -> 10)
            | _ -> 10
          in
          if radix = 16 then Ok (Printf.sprintf "%x" v)
          else Ok (string_of_int v)
      | Error err -> Error ("eval: " ^ err)

let dummy_control _ctx _args = Ok ""

let register_defaults reg =
  (* 制御マクロもビルトインとして登録 *)
  reg "define" dummy_control;
  reg "undefine" dummy_control;
  reg "ifdef" dummy_control;
  reg "ifelse" dummy_control;
  reg "changequote" dummy_control;
  reg "changecom" dummy_control;
  reg "dnl" dummy_control;
  reg "shift" dummy_control;
  reg "defn" dummy_control;

  (* 標準 POSIX ビルトイン *)
  reg "len" builtin_len;
  reg "index" builtin_index;
  reg "substr" builtin_substr;
  reg "translit" builtin_translit;
  reg "incr" builtin_incr;
  reg "decr" builtin_decr;
  reg "eval" builtin_eval;

  (* エスケープ関数 *)
  reg "h" (fun _ args -> Ok (match args with s :: _ -> html_escape s | [] -> ""));
  reg "html_escape" (fun _ args -> Ok (match args with s :: _ -> html_escape s | [] -> ""));
  reg "json_escape" (fun _ args -> Ok (match args with s :: _ -> json_escape s | [] -> ""));
  reg "sh_quote" (fun _ args -> Ok (match args with s :: _ -> sh_quote s | [] -> "''"))

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

let builtin_divert ctx args =
  let div =
    match args with
    | s :: _ when s <> "" -> (try int_of_string (String.trim s) with _ -> 0)
    | _ -> 0
  in
  ctx.set_current_div div;
  Ok ""

let builtin_undivert ctx args =
  let targets =
    match args with
    | [] ->
        let keys = ctx.get_all_diversion_keys () in
        List.sort compare (List.filter (fun k -> k > 0) keys)
    | _ ->
        List.filter_map (fun s ->
          try
            let k = int_of_string (String.trim s) in
            if k > 0 then Some k else None
          with _ -> None
        ) args
  in
  let buf = Buffer.create 128 in
  List.iter (fun d ->
    let div_buf = ctx.get_diversion d in
    Buffer.add_buffer buf div_buf;
    ctx.remove_diversion d
  ) targets;
  Ok (Buffer.contents buf)

let check_sandbox root path =
  if Filename.is_relative path then
    let full = Filename.concat root path in
    (* 簡易パストラバーサル検出 *)
    let segs = String.split_on_char '/' path in
    let depth = ref 0 in
    let violated = ref false in
    List.iter (fun seg ->
      if seg = ".." then begin
        decr depth;
        if !depth < 0 then violated := true
      end else if seg <> "." && seg <> "" then
        incr depth
    ) segs;
    if !violated then Error (Printf.sprintf "security violation: path traversal detected in include(%s)" path)
    else Ok full
  else
    Error (Printf.sprintf "security violation: absolute paths not allowed in include(%s)" path)

let builtin_include_common is_silent ctx args =
  match args with
  | [] | [""] ->
      if is_silent then Ok "" else Error "include: missing file path"
  | path :: _ ->
      match check_sandbox (ctx.get_sandbox_root ()) (String.trim path) with
      | Error err -> if is_silent then Ok "" else Error err
      | Ok full_path ->
          try
            let ch = open_in full_path in
            let len = in_channel_length ch in
            let s = really_input_string ch len in
            close_in ch;
            Ok s
          with _ ->
            if is_silent then Ok ""
            else Error (Printf.sprintf "include(%s): file not found or inaccessible" path)

let builtin_include ctx args = builtin_include_common false ctx args
let builtin_sinclude ctx args = builtin_include_common true ctx args

(* POSIX eval 実装: ビット演算、論理演算、比較、四則演算 *)
let eval_ast expr_str =
  let s = String.trim expr_str in
  try
    let len = String.length s in
    let pos = ref 0 in
    let peek_char () = if !pos < len then Some s.[!pos] else None in
    let get_char () =
      if !pos < len then let c = s.[!pos] in incr pos; Some c else None
    in
    let rec skip_ws () =
      match peek_char () with
      | Some (' ' | '\t' | '\n' | '\r') -> incr pos; skip_ws ()
      | _ -> ()
    in

    let rec parse_primary () =
      skip_ws ();
      match peek_char () with
      | Some '(' ->
          incr pos;
          let v = parse_logical_or () in
          skip_ws ();
          (match get_char () with
           | Some ')' -> v
           | _ -> failwith "mismatched parenthesis")
      | Some '-' ->
          incr pos;
          -(parse_primary ())
      | Some '+' ->
          incr pos;
          parse_primary ()
      | Some '~' ->
          incr pos;
          lnot (parse_primary ())
      | Some '!' ->
          incr pos;
          if parse_primary () = 0 then 1 else 0
      | Some ('0'..'9') ->
          let start_p = !pos in
          while match peek_char () with Some ('0'..'9') -> true | _ -> false do
            incr pos
          done;
          int_of_string (String.sub s start_p (!pos - start_p))
      | _ -> failwith "syntax error in number"

    and parse_multiplicative () =
      let rec loop acc =
        skip_ws ();
        match peek_char () with
        | Some '*' -> incr pos; loop (acc * parse_primary ())
        | Some '/' ->
            incr pos;
            let rhs = parse_primary () in
            if rhs = 0 then failwith "division by zero" else loop (acc / rhs)
        | Some '%' ->
            incr pos;
            let rhs = parse_primary () in
            if rhs = 0 then failwith "division by zero" else loop (acc mod rhs)
        | _ -> acc
      in
      loop (parse_primary ())

    and parse_additive () =
      let rec loop acc =
        skip_ws ();
        match peek_char () with
        | Some '+' -> incr pos; loop (acc + parse_multiplicative ())
        | Some '-' -> incr pos; loop (acc - parse_multiplicative ())
        | _ -> acc
      in
      loop (parse_multiplicative ())

    and parse_shift () =
      let rec loop acc =
        skip_ws ();
        if !pos + 1 < len && s.[!pos] = '<' && s.[!pos+1] = '<' then begin
          pos := !pos + 2;
          loop (acc lsl parse_additive ())
        end else if !pos + 1 < len && s.[!pos] = '>' && s.[!pos+1] = '>' then begin
          pos := !pos + 2;
          loop (acc lsr parse_additive ())
        end else acc
      in
      loop (parse_additive ())

    and parse_relational () =
      let lhs = parse_shift () in
      skip_ws ();
      if !pos + 1 < len && s.[!pos] = '<' && s.[!pos+1] = '=' then begin
        pos := !pos + 2; if lhs <= parse_shift () then 1 else 0
      end else if !pos + 1 < len && s.[!pos] = '>' && s.[!pos+1] = '=' then begin
        pos := !pos + 2; if lhs >= parse_shift () then 1 else 0
      end else match peek_char () with
      | Some '<' -> incr pos; if lhs < parse_shift () then 1 else 0
      | Some '>' -> incr pos; if lhs > parse_shift () then 1 else 0
      | _ -> lhs

    and parse_equality () =
      let lhs = parse_relational () in
      skip_ws ();
      if !pos + 1 < len && s.[!pos] = '=' && s.[!pos+1] = '=' then begin
        pos := !pos + 2; if lhs = parse_relational () then 1 else 0
      end else if !pos + 1 < len && s.[!pos] = '!' && s.[!pos+1] = '=' then begin
        pos := !pos + 2; if lhs <> parse_relational () then 1 else 0
      end else lhs

    and parse_bitwise_and () =
      let rec loop acc =
        skip_ws ();
        if !pos < len && s.[!pos] = '&' && (!pos + 1 >= len || s.[!pos+1] <> '&') then begin
          incr pos;
          loop (acc land parse_equality ())
        end else acc
      in
      loop (parse_equality ())

    and parse_bitwise_xor () =
      let rec loop acc =
        skip_ws ();
        match peek_char () with
        | Some '^' -> incr pos; loop (acc lxor parse_bitwise_and ())
        | _ -> acc
      in
      loop (parse_bitwise_and ())

    and parse_bitwise_or () =
      let rec loop acc =
        skip_ws ();
        if !pos < len && s.[!pos] = '|' && (!pos + 1 >= len || s.[!pos+1] <> '|') then begin
          incr pos;
          loop (acc lor parse_bitwise_xor ())
        end else acc
      in
      loop (parse_bitwise_xor ())

    and parse_logical_and () =
      let rec loop acc =
        skip_ws ();
        if !pos + 1 < len && s.[!pos] = '&' && s.[!pos+1] = '&' then begin
          pos := !pos + 2;
          let rhs = parse_bitwise_or () in
          loop (if acc <> 0 && rhs <> 0 then 1 else 0)
        end else acc
      in
      loop (parse_bitwise_or ())

    and parse_logical_or () =
      let rec loop acc =
        skip_ws ();
        if !pos + 1 < len && s.[!pos] = '|' && s.[!pos+1] = '|' then begin
          pos := !pos + 2;
          let rhs = parse_logical_and () in
          loop (if acc <> 0 || rhs <> 0 then 1 else 0)
        end else acc
      in
      loop (parse_logical_and ())
    in

    let res = parse_logical_or () in
    skip_ws ();
    if !pos < len then failwith "unexpected tokens at end of expression"
    else Ok res
  with e -> Error (Printexc.to_string e)

let builtin_eval _ctx args =
  match args with
  | [] -> Ok "0"
  | expr :: rest ->
      match eval_ast expr with
      | Ok v ->
          let radix =
            match rest with
            | r_str :: _ when r_str <> "" -> (try int_of_string (String.trim r_str) with _ -> 10)
            | _ -> 10
          in
          if radix = 16 then Ok (Printf.sprintf "%x" v)
          else if radix = 8 then Ok (Printf.sprintf "%o" v)
          else if radix = 2 then
            let rec to_bin n =
              if n = 0 then "" else (to_bin (n lsr 1)) ^ (string_of_int (n land 1))
            in
            if v = 0 then Ok "0" else Ok (to_bin v)
          else Ok (string_of_int v)
      | Error err -> Error ("eval: " ^ err)

let dummy_control _ctx _args = Ok ""

let register_defaults reg =
  reg "define" dummy_control;
  reg "undefine" dummy_control;
  reg "ifdef" dummy_control;
  reg "ifelse" dummy_control;
  reg "changequote" dummy_control;
  reg "changecom" dummy_control;
  reg "dnl" dummy_control;
  reg "shift" dummy_control;
  reg "defn" dummy_control;

  reg "divert" builtin_divert;
  reg "undivert" builtin_undivert;
  reg "include" builtin_include;
  reg "sinclude" builtin_sinclude;

  reg "len" builtin_len;
  reg "index" builtin_index;
  reg "substr" builtin_substr;
  reg "translit" builtin_translit;
  reg "incr" builtin_incr;
  reg "decr" builtin_decr;
  reg "eval" builtin_eval;

  reg "h" (fun _ args -> Ok (match args with s :: _ -> html_escape s | [] -> ""));
  reg "html_escape" (fun _ args -> Ok (match args with s :: _ -> html_escape s | [] -> ""));
  reg "json_escape" (fun _ args -> Ok (match args with s :: _ -> json_escape s | [] -> ""));
  reg "sh_quote" (fun _ args -> Ok (match args with s :: _ -> sh_quote s | [] -> "''"))

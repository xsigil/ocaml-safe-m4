type token_type =
  | TokenEOF
  | TokenIdent of string
  | TokenQuoted of string
  | TokenComment of string
  | TokenOther of char

type t = {
  mutable stack : string list;
  mutable left_quote : string;
  mutable right_quote : string;
  mutable left_comment : string;
  mutable right_comment : string;
}

let create () = {
  stack = [];
  left_quote = "`";
  right_quote = "'";
  left_comment = "#";
  right_comment = "\n";
}

let push_input t s =
  if s <> "" then t.stack <- s :: t.stack

let rec read_char t =
  match t.stack with
  | [] -> None
  | "" :: rest ->
      t.stack <- rest;
      read_char t
  | top :: rest ->
      let c = top.[0] in
      let sub = String.sub top 1 (String.length top - 1) in
      t.stack <- sub :: rest;
      Some c

let unread_char t c =
  push_input t (String.make 1 c)

let match_prefix t prefix =
  if prefix = "" then false
  else
    let len = String.length prefix in
    let read_chars = ref [] in
    let matched = ref true in
    for i = 0 to len - 1 do
      if !matched then
        match read_char t with
        | None -> matched := false
        | Some c ->
            read_chars := c :: !read_chars;
            if c <> prefix.[i] then matched := false
    done;
    if !matched then true
    else begin
      List.iter (unread_char t) !read_chars;
      false
    end

let read_until_matching t open_delim close_delim =
  let buf = Buffer.create 32 in
  let depth = ref 1 in
  let continue_reading = ref true in
  while !continue_reading do
    if match_prefix t open_delim then begin
      incr depth;
      Buffer.add_string buf open_delim
    end else if match_prefix t close_delim then begin
      decr depth;
      if !depth = 0 then continue_reading := false
      else Buffer.add_string buf close_delim
    end else
      match read_char t with
      | None -> continue_reading := false
      | Some c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let read_comment t rc =
  let buf = Buffer.create 32 in
  let delim = if rc = "" then "\n" else rc in
  let continue_reading = ref true in
  while !continue_reading do
    if match_prefix t delim then begin
      Buffer.add_string buf delim;
      continue_reading := false
    end else
      match read_char t with
      | None -> continue_reading := false
      | Some c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_part c =
  is_ident_start c || (c >= '0' && c <= '9')

let next_token t =
  if t.left_comment <> "" && match_prefix t t.left_comment then
    let content = read_comment t t.right_comment in
    TokenComment (t.left_comment ^ content)
  else if match_prefix t t.left_quote then
    let content = read_until_matching t t.left_quote t.right_quote in
    TokenQuoted content
  else
    match read_char t with
    | None -> TokenEOF
    | Some c ->
        if is_ident_start c then
          let buf = Buffer.create 16 in
          Buffer.add_char buf c;
          let continue_ident = ref true in
          while !continue_ident do
            match read_char t with
            | None -> continue_ident := false
            | Some nc ->
                if is_ident_part nc then Buffer.add_char buf nc
                else begin
                  unread_char t nc;
                  continue_ident := false
                end
          done;
          TokenIdent (Buffer.contents buf)
        else
          TokenOther c

let set_quotes t l r =
  t.left_quote <- l;
  t.right_quote <- r

let set_comments t l r =
  t.left_comment <- l;
  t.right_comment <- r

let get_quotes t = (t.left_quote, t.right_quote)

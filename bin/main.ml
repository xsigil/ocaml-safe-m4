open Safe_m4

let parse_define def =
  try
    let eq_idx = String.index def '=' in
    let k = String.sub def 0 eq_idx in
    let v = String.sub def (eq_idx + 1) (String.length def - eq_idx - 1) in
    (k, v)
  with Not_found -> (def, "")

let () =
  let defines = ref [] in
  let files = ref [] in
  let sandbox = ref "." in

  let rec parse_args idx =
    if idx >= Array.length Sys.argv then ()
    else
      let arg = Sys.argv.(idx) in
      if arg = "-D" && idx + 1 < Array.length Sys.argv then begin
        defines := parse_define Sys.argv.(idx + 1) :: !defines;
        parse_args (idx + 2)
      end else if String.length arg > 2 && String.sub arg 0 2 = "-D" then begin
        defines := parse_define (String.sub arg 2 (String.length arg - 2)) :: !defines;
        parse_args (idx + 1)
      end else if arg = "--sandbox" && idx + 1 < Array.length Sys.argv then begin
        sandbox := Sys.argv.(idx + 1);
        parse_args (idx + 2)
      end else begin
        files := arg :: !files;
        parse_args (idx + 1)
      end
  in
  parse_args 1;

  let eng = Engine.create ~sandbox_root:!sandbox () in

  List.iter (fun (k, v) ->
    Engine.define eng k v
  ) (List.rev !defines);

  let run_input input =
    match Engine.expand eng input with
    | Ok res -> print_string res
    | Error err ->
        prerr_endline ("safe-m4: " ^ err);
        exit 1
  in

  match List.rev !files with
  | [] ->
      let buf = Buffer.create 4096 in
      let chunk = Bytes.create 4096 in
      let rec loop () =
        let n = input stdin chunk 0 4096 in
        if n > 0 then begin
          Buffer.add_subbytes buf chunk 0 n;
          loop ()
        end
      in
      loop ();
      run_input (Buffer.contents buf)
  | file_list ->
      List.iter (fun f ->
        let ch = open_in f in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        run_input s
      ) file_list

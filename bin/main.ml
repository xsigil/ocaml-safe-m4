open Safe_m4

let () =
  let eng = Engine.create () in
  let input =
    if Array.length Sys.argv > 1 then
      let ch = open_in Sys.argv.(1) in
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      s
    else
      let buf = Buffer.create 512 in
      (try
        while true do
          Buffer.add_string buf (input_line stdin);
          Buffer.add_char buf '\n'
        done
       with End_of_file -> ());
      Buffer.contents buf
  in
  match Engine.expand eng input with
  | Ok res -> print_string res
  | Error err ->
      prerr_endline ("safe-m4: " ^ err);
      exit 1

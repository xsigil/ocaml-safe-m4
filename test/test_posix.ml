open Safe_m4

let format_diff got want =
  let max_len = max (String.length got) (String.length want) in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\n--- CHAR DIFF ---\n";
  for i = 0 to max_len - 1 do
    let g_str = if i < String.length got then Printf.sprintf "%C (0x%02X)" got.[i] (Char.code got.[i]) else "<EOF>" in
    let w_str = if i < String.length want then Printf.sprintf "%C (0x%02X)" want.[i] (Char.code want.[i]) else "<EOF>" in
    let status = if i < String.length got && i < String.length want && got.[i] = want.[i] then "OK" else "MISMATCH" in
    Buffer.add_string buf (Printf.sprintf "[%02d] GOT: %-15s | WANT: %-15s | %s\n" i g_str w_str status)
  done;
  Buffer.contents buf

let assert_equal name got want =
  if got = want then
    Printf.printf "PASS: %s\n" name
  else begin
    Printf.printf "\n=== FAILURE: %s ===\nGOT : %S\nWANT: %S%s\n" name got want (format_diff got want);
    exit 1
  end

let () =
  let run name input expected =
    let eng = Engine.create () in
    match Engine.expand eng input with
    | Ok got -> assert_equal name got expected
    | Error err ->
        Printf.printf "[ERR ] %s: %s\n" name err;
        exit 1
  in

  print_endline "=== Running OCaml Safe-M4 Test Suite ===";
  run "simple define" "define(`FOO', `bar')FOO" "bar";
  run "macro with args" "define(`GREET', `Hello, $1!')GREET(`World')" "Hello, World!";
  run "ifelse match" "ifelse(`a', `a', `YES', `NO')" "YES";
  run "ifelse mismatch" "ifelse(`a', `b', `YES', `NO')" "NO";
  
  (* Goで苦戦した: 先行展開 (eager evaluation) の検証 *)
  run "eval arithmetic" "eval(`(10 + 20) * 2 - 5')" "55";
  run "eval with ifelse" "ifelse(eval(`10 > 5'), 1, `YES', `NO')" "YES";
  
  (* Goで苦戦した: コメント透過と changecom *)
  run "comments pass through" "define(`FOO', `bar')# FOO is not expanded\nFOO" "# FOO is not expanded\nbar";
  run "changecom custom" "define(`FOO', `bar')changecom(`/*', `*/')/* FOO */ FOO" "/* FOO */ bar";

  (* 範囲置換 *)
  run "translit range" "translit(`hello 123', `a-z', `A-Z')" "HELLO 123";

  (* Raw 出力ルーティング *)
  run "html escape" "h(`<script>alert(1)</script>')" "&lt;script&gt;alert(1)&lt;/script&gt;";
  run "shell quote" "sh_quote(`echo \"test\"')" "'echo \"test\"'";

  (* クォート保持 *)
  run "nested quotes" "define(`WRAP', `[$1]')WRAP(```inner quote''')" "[`inner quote']";
  run "dnl newline" "define(`FOO', `bar')dnl\nFOO" "bar";

  print_endline "=== All OCaml tests passed! ==="

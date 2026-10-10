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
  let run ?sandbox name input expected =
    let eng = match sandbox with Some s -> Engine.create ~sandbox_root:s () | None -> Engine.create () in
    match Engine.expand eng input with
    | Ok got -> assert_equal name got expected
    | Error err ->
        Printf.printf "[ERR ] %s: %s\n" name err;
        exit 1
  in

  let run_fail ?sandbox name input expect_err_sub =
    let eng = match sandbox with Some s -> Engine.create ~sandbox_root:s () | None -> Engine.create () in
    match Engine.expand eng input with
    | Ok got ->
        Printf.printf "\n=== EXPECTED ERROR BUT GOT SUCCESS: %s ===\nGOT : %S\n" name got;
        exit 1
    | Error err ->
        if (try ignore (Str.search_forward (Str.regexp_string expect_err_sub) err 0); true with Not_found -> false) then
          Printf.printf "PASS (Expected Error): %s\n" name
        else begin
          Printf.printf "\n=== UNEXPECTED ERROR: %s ===\nGOT ERROR: %S\nWANT SUBSTRING: %S\n" name err expect_err_sub;
          exit 1
        end
  in

  print_endline "=== Running OCaml Safe-M4 Full POSIX Test Suite ===";
  run "simple define" "define(`FOO', `bar')FOO" "bar";
  run "macro with args" "define(`GREET', `Hello, $1!')GREET(`World')" "Hello, World!";
  run "ifelse match" "ifelse(`a', `a', `YES', `NO')" "YES";
  run "ifelse mismatch" "ifelse(`a', `b', `YES', `NO')" "NO";
  
  (* Eager Evaluation *)
  run "eval arithmetic" "eval(`(10 + 20) * 2 - 5')" "55";
  run "eval with ifelse" "ifelse(eval(`10 > 5'), 1, `YES', `NO')" "YES";
  run "eval bitwise & logic" "eval(`(1 << 3) | 2 & ~0')" "10";
  run "eval logic short" "eval(`10 > 5 && 2 < 4')" "1";

  (* Comments & Changecom *)
  run "comments pass through" "define(`FOO', `bar')# FOO is not expanded\nFOO" "# FOO is not expanded\nbar";
  run "changecom custom" "define(`FOO', `bar')changecom(`/*', `*/')/* FOO */ FOO" "/* FOO */ bar";

  (* Diversion & Undivert *)
  run "diversions test" "divert(1)diverted text\ndivert(0)initial text\nundivert(1)" "initial text\ndiverted text\n";

  (* Translit, Substr, Index *)
  run "translit range" "translit(`hello 123', `a-z', `A-Z')" "HELLO 123";
  run "substr test" "substr(`gnus and gnats', `9', `5')" "gnats";
  run "index test" "index(`gnus and gnats', `gnat')" "9";

  (* Raw Escaping *)
  run "html escape" "h(`<script>alert(1)</script>')" "&lt;script&gt;alert(1)&lt;/script&gt;";
  run "shell quote" "sh_quote(`echo \"test\"')" "'echo \"test\"'";

  (* Nested Quotes *)
  run "nested quotes" "define(`WRAP', `[$1]')WRAP(```inner quote''')" "[`inner quote']";
  run "dnl newline" "define(`FOO', `bar')dnl\nFOO" "bar";

  (* Sandbox Security *)
  run_fail ~sandbox:"/tmp" "traversal blocked" "include(`../../etc/passwd')" "security violation";

  print_endline "=== All OCaml tests passed! ==="

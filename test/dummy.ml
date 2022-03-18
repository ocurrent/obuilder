let () =
  Printexc.record_backtrace true;
  let str = "the quick brown fox jumps over the lazy dog" in
  match Sys.argv.(1) with
  | "stdin" -> if read_line () <> str then exit 1
  | "stdout" -> print_string str
  | "stderr" -> prerr_string str
  | _ -> invalid_arg "Sys.argv"

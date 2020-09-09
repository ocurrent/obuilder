open Lwt.Infix

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_cmd = Fmt.(Dump.list Dump.string)

let exec ?cwd ?stdin ?stdout ?stderr argv =
  Lwt_process.exec ?cwd ?stdin ?stdout ?stderr ("", Array.of_list argv) >|= function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Fmt.failwith "%a failed with exit status %d" pp_cmd argv n
  | Unix.WSIGNALED x -> Fmt.failwith "%a failed with signal %d" pp_cmd argv x
  | Unix.WSTOPPED x -> Fmt.failwith "%a stopped with signal %a" pp_cmd argv pp_signal x

(* Collect log data from builds, for unit-tests. *)

open Lwt.Infix

type t = {
  label : string;
  buf : Buffer.t;
  cond : unit Lwt_condition.t;
}

let create label =
  let buf = Buffer.create 1024 in
  let cond = Lwt_condition.create () in
  { label; buf; cond }

let add t tag x =
  Logs.info (fun f -> f "%s: %S" t.label x);
  begin match tag with
    | `Heading -> Buffer.add_string t.buf (x ^ "\n")
    | `Note -> Buffer.add_string t.buf (";" ^ x ^ "\n")
    | `Output -> Buffer.add_string t.buf x
  end;
  Lwt_condition.broadcast t.cond ()

let contents t =
  Buffer.contents t.buf

let clear t =
  Buffer.clear t.buf

let remove_notes x =
  x
  |> String.split_on_char '\n'
  |> List.filter (fun x -> not (Astring.String.is_prefix ~affix:";" x))
  |> String.concat "\n"

let rec await t expect =
  let got = Buffer.contents t.buf |> remove_notes in
  if got = expect then Lwt.return_unit
  else if String.length got > String.length expect then (
      Fmt.failwith "Log expected %S but got %S" expect got
  ) else (
    let common = min (String.length expect) (String.length got) in
    if String.sub got 0 common = String.sub expect 0 common then (
      Lwt_condition.wait t.cond >>= fun () ->
      await t expect
    ) else (
      Fmt.failwith "Log expected %S but got %S" expect got
    )
  )

let check name pattern t =
  let pattern = String.split_on_char '\n' pattern |> List.map String.trim |> String.concat "\n" in
  let re = Str.regexp pattern in
  let got = contents t in
  if not (Str.string_match re got 0) then
    Alcotest.(check string) name pattern got

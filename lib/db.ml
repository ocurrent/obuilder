type t = Sqlite3.db

let or_fail db ~cmd x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3: [%s] %s (executing %S)" (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db) cmd

let no_callback _ = failwith "[exec] used with a query!"

let exec_stmt db ?(cb=no_callback) stmt =
  let rec loop () =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> ()
    | Sqlite3.Rc.ROW ->
      let cols = Sqlite3.data_count stmt in
      cb @@ List.init cols (fun i -> Sqlite3.column stmt i);
      loop ()
    | x -> Fmt.failwith "Sqlite3 exec error: [%s] %s" (Sqlite3.Rc.to_string x) (Sqlite3.errmsg db)
  in
  loop ()

let exec_literal db sql =
  Sqlite3.exec db sql |> or_fail db ~cmd:sql

let bind db stmt values =
  Sqlite3.reset stmt |> or_fail db ~cmd:"reset";
  List.iteri (fun i v -> Sqlite3.bind stmt (i + 1) v |> or_fail db ~cmd:"bind") values

let exec db stmt values =
  bind db stmt values;
  exec_stmt db stmt

let query db stmt values =
  bind db stmt values;
  let results = ref [] in
  let cb row =
    results := row :: !results
  in
  exec_stmt db ~cb stmt;
  List.rev !results

let query_one db stmt values =
  match query db stmt values with
  | [row] -> row
  | [] -> failwith "No results from SQL query!"
  | _ -> failwith "Multiple results from SQL query!"

let query_some db stmt values =
  match query db stmt values with
  | [] -> None
  | [row] -> Some row
  | _ -> failwith "Multiple results from SQL query!"

let of_dir path =
  let db = Sqlite3.db_open path in
  Sqlite3.busy_timeout db 1000;
  exec_literal db "PRAGMA journal_mode=WAL";
  exec_literal db "PRAGMA synchronous=NORMAL";
  db

let dump_item = Fmt.of_to_string Sqlite3.Data.to_string_debug
let dump_row = Fmt.(Dump.list dump_item)

let close db =
  if not (Sqlite3.db_close db) then
    Fmt.failwith "Could not close database! It is busy."

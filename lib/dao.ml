type t = {
  db : Db.t;
  begin_transaction : Sqlite3.stmt;
  commit : Sqlite3.stmt;
  rollback : Sqlite3.stmt;
  add : Sqlite3.stmt;
  set_used : Sqlite3.stmt;
  update_rc : Sqlite3.stmt;
  exists : Sqlite3.stmt;
  children : Sqlite3.stmt;
  delete : Sqlite3.stmt;
  lru : Sqlite3.stmt;
  parent : Sqlite3.stmt;
  count : Sqlite3.stmt;
}

let format_timestamp time =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
  Fmt.str "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let create db =
  Sqlite3.exec db {| CREATE TABLE IF NOT EXISTS builds (
                       id       TEXT PRIMARY KEY,
                       created  DATETIME NOT NULL,
                       used     DATETIME NOT NULL,
                       rc       INTEGER NOT NULL,
                       parent   TEXT,
                       FOREIGN KEY (parent) REFERENCES builds (id) ON DELETE RESTRICT
                     ) |} |> Db.or_fail db ~cmd:"create builds";
  Sqlite3.exec db {| CREATE INDEX IF NOT EXISTS lru
                     ON builds (rc, used) |} |> Db.or_fail db ~cmd:"create lru index";
  let begin_transaction = Sqlite3.prepare db "BEGIN TRANSACTION" in
  let commit = Sqlite3.prepare db "COMMIT" in
  let rollback = Sqlite3.prepare db {| ROLLBACK |} in
  let add = Sqlite3.prepare db {| INSERT INTO builds
                                    (id, created, used, rc, parent)
                                    VALUES (?, ?, ?, 0, ?) |} in
  let update_rc = Sqlite3.prepare db {| UPDATE builds SET rc = rc + ? WHERE id = ? |} in
  let set_used = Sqlite3.prepare db {| UPDATE builds SET used = ? WHERE id = ? |} in
  let exists = Sqlite3.prepare db {| SELECT EXISTS(SELECT 1 FROM builds WHERE id = ?) |} in
  let children = Sqlite3.prepare db {| SELECT id FROM builds WHERE parent = ? |} in
  let delete = Sqlite3.prepare db {| DELETE FROM builds WHERE id = ? |} in
  let lru = Sqlite3.prepare db {| SELECT id FROM builds WHERE rc = 0 AND used < ? ORDER BY used ASC LIMIT ? |} in
  let parent = Sqlite3.prepare db {| SELECT parent FROM builds WHERE id = ? |} in
  let count = Sqlite3.prepare db {| SELECT COUNT(*) FROM builds |} in
  { db; begin_transaction; commit; rollback; add; set_used; update_rc; exists; children; delete; lru; parent; count }

let with_transaction t fn =
  Db.exec t.db t.begin_transaction [];
  match fn () with
  | x -> Db.exec t.db t.commit []; x
  | exception ex -> Db.exec t.db t.rollback []; raise ex

let add ?parent ~id ~now t =
  let now = format_timestamp now in
  match parent with
  | None -> Db.exec t.db t.add Sqlite3.Data.[ TEXT id; TEXT now; TEXT now; NULL ];
  | Some parent ->
    with_transaction t (fun () ->
        Db.exec t.db t.add Sqlite3.Data.[ TEXT id; TEXT now; TEXT now; TEXT parent ];
        Db.exec t.db t.update_rc Sqlite3.Data.[ INT 1L; TEXT parent ];
      )

let set_used ~id ~now t =
  let now = format_timestamp now in
  Db.exec t.db t.set_used Sqlite3.Data.[ TEXT now; TEXT id ]

let children t id =
  match Db.query_one t.db t.exists Sqlite3.Data.[ TEXT id ] with
  | [ INT 0L ] -> Error `No_such_id
  | [ INT 1L ] ->
    Db.query t.db t.children Sqlite3.Data.[ TEXT id ] |> List.map (function
        | Sqlite3.Data.[ TEXT dep ] -> dep
        | x -> Fmt.failwith "Invalid row: %a" Db.dump_row x
      )
    |> Result.ok
  | x -> Fmt.failwith "Invalid row: %a" Db.dump_row x

let delete t id =
  with_transaction t (fun () ->
      match Db.query_one t.db t.parent Sqlite3.Data.[ TEXT id ] with
      | [ TEXT parent ] ->
        Db.exec t.db t.delete Sqlite3.Data.[ TEXT id ];
        Db.exec t.db t.update_rc Sqlite3.Data.[ INT (-1L); TEXT parent ]
      | [ NULL ] ->
        Db.exec t.db t.delete Sqlite3.Data.[ TEXT id ]
      | x -> Fmt.failwith "Invalid row: %a" Db.dump_row x
    )

let lru t ~before n =
  Db.query t.db t.lru Sqlite3.Data.[ TEXT (format_timestamp before); INT (Int64.of_int n) ]
  |> List.map @@ function
  | Sqlite3.Data.[ TEXT id ] -> id
  | x -> Fmt.failwith "Invalid row: %a" Db.dump_row x

let count t =
  match Db.query_one t.db t.count [] with
  | [ INT n ] -> n
  | x -> Fmt.failwith "Invalid row: %a" Db.dump_row x

let close t =
  Sqlite3.finalize t.begin_transaction |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.commit |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.rollback |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.add |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.set_used |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.update_rc |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.exists |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.children |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.delete |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.lru |> Db.or_fail t.db ~cmd:"finalize";
  Sqlite3.finalize t.parent |> Db.or_fail t.db ~cmd:"finalize";
  Db.close t.db

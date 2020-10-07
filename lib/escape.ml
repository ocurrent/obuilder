(* Make a cache name safe to use as a filename.
   Different inputs must always produce different outputs.
   The output string will match /[-._A-Za-z0-9%]+/. *)
let cache x =
  let b = Buffer.create (String.length x * 2) in
  Buffer.add_string b "c-";
  x |> String.iter (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' as c -> Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "%%%x" (Char.code c))
    );
  Buffer.contents b

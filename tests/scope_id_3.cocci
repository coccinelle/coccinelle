@initialize:ocaml@
@@

let offset = ref 0

let split  = function
    "" -> ""
  | s ->
      (if !offset = 0
      then
        let start = Str.search_forward (Str.regexp "linux-") s 0 in
	offset := start);
      String.sub s !offset (String.length s - !offset)

@r@
int e;
position p;
@@

e@p

@script:ocaml@
e << r.e;
p << r.p;
@@

Printf.printf "%s: %s: %s: %d\n" e
    (String.concat " " (List.map split (files()))) (split (List.hd p).file)
    (List.hd p).line

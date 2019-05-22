@r@
comments c1,c2,c3,c4;
statement S;
type T;
@@

T@c1 main(...)@c2 {
  ... when any
  \(foo@c3();\&S@c4\)
  ... when any
}

@script:ocaml@
c1 << r.c1;
c2 << r.c2;
c3 << r.c3;
c4 << r.c4;
@@

let (c1b,c1m,c1a) = List.hd c1 in
let (c2b,c2m,c2a) = List.hd c2 in
let (c3b,c3m,c3a) = List.hd c3 in
let (c4b,c4m,c4a) = List.hd c4 in
Printf.printf "c1b: %s\n" (String.concat " " c1b);
Printf.printf "c1m: %s\n" (String.concat " " c1m);
Printf.printf "c1a: %s\n\n" (String.concat " " c1a);
Printf.printf "c2b: %s\n" (String.concat " " c2b);
Printf.printf "c2m: %s\n" (String.concat " " c2m);
Printf.printf "c2a: %s\n\n" (String.concat " " c2a);
Printf.printf "c3b: %s\n" (String.concat " " c3b);
Printf.printf "c3m: %s\n" (String.concat " " c3m);
Printf.printf "c3a: %s\n\n" (String.concat " " c3a);
Printf.printf "c4b: %s\n" (String.concat " " c4b);
Printf.printf "c4m: %s\n" (String.concat " " c4m);
Printf.printf "c4a: %s\n\n" (String.concat " " c4a);
Printf.printf "-------------------\n"
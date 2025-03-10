@r@
comments c;
position p;
@@

foo@p();@c

@script:ocaml@
(_,cs) << r.c;
p << r.p;
@@

let (_,_,cas) = List.hd cs in
Printf.eprintf "%d\n" (List.length cas);
(if List.length cas > 1 then Coccilib.include_match false)

@depends on r@
@@

- foo();
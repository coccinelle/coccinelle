@initialize:ocaml@
@@

let past_line_4(p,other) =
    (List.hd p).line > 4

@r@
expression e;
@@

f(e)

@@
position p : script:ocaml(r.e) { past_line_4(p,e) };
expression r.e;
@@

g(
-e@p
+27
 )

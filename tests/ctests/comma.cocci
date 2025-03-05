@r@
expression e1,e2;
statement S;
position p;
@@

e1 ,@S@p e2;

@@
expression e1,e2;
position p1;
position p2 :
    script:ocaml(p1) { not((List.hd p1).line_end = (List.hd p2).line) };
statement S;
position r.p;
@@

e1@p1
-,@S@p
+;
e2@p2
... when any

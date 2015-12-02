@r@
identifier f;
expression e;
position p;
@@

(
other
|
f@p
)
  (
(
nothing
|
e
)
  );

@script:ocaml@
f << r.f;
e << r.e;
p << r.p;
@@

Printf.printf "all matched: %s %s %s %d\n"
   f e (List.hd p).file (List.hd p).line

@script:ocaml@
f << r.f = "no function";
e << r.e = "no argument";
p << r.p = [];
@@

match p with
  [] -> Printf.printf "no pos: %s %s\n" f e
| p::_ -> Printf.printf "all matched: %s %s %s %d\n" f e p.file p.line

@script:ocaml@
f << r.f;
e << r.e = "no argument";
p << r.p = [];
@@

match p with
  [] -> Printf.printf "fn required: no pos: %s %s\n" f e
| p::_ ->
    Printf.printf "fn required: all matched: %s %s %s %d\n"
      f e p.file p.line

@script:python@
f << r.f;
e << r.e;
p << r.p;
@@

print "py: all matched: %s %s %s %s" % (f,e,p[0].file,p[0].line)

@script:python@
f << r.f = "no function";
e << r.e = "no argument";
p << r.p = [];
@@

if not p:
  print "py: no pos: %s %s" % (f,e)
else:
  print "py: all matched: %s %s %s %s" % (f,e,p[0].file,p[0].line)

@script:python@
f << r.f;
e << r.e = "no argument";
p << r.p = [];
@@

if not p:
  print "py: fun required: no pos: %s %s" % (f,e)
else:
  print "py: fun required: all matched: %s %s %s %s" % (f,e,p[0].file,p[0].line)

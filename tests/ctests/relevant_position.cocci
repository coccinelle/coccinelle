@r@
position p1;
@@

struct blah@p1 { ... }

@s@
position p2;
@@

struct sk_buff@p2 { ... }

@script:ocaml@
p1 << r.p1;
p2 << s.p2;
@@

if (List.hd p1).current_element_line = (List.hd p2).current_element_line
then Coccilib.include_match false

@@
position r.p1;
@@

- struct blah@p1 { ... };

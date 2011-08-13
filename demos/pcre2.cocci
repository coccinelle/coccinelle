@m@
identifier SPAM ;
@@

SPAM

@script:ocaml r@
s << m.SPAM;
new_s;
@@

let re = Pcre.regexp "(WINE_)?(ERR|FIXME|WARN)" in
if Pcre.pmatch ~rex:re s then
(Printf.eprintf "Match %s\n" s;
 new_s := Pcre.replace_first ~pat:"WINE_" s
)
else
(Printf.eprintf "Not match %s\n" s;
 Coccilib.include_match false
)

@@
identifier m.SPAM;
identifier r.new_s;
@@

-SPAM
+new_s

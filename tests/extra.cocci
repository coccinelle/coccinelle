@r@
identifier f;
fresh identifier ff = "_called_function_";
parameter list pl;
type T;
@@

  T 
- f
+ ff
  (pl) { ... }
+ T f(pl) { return ff(); }

@script:ocaml s@
(_,pl) << r.pl;
ff << r.ff;
newargs;
@@

newargs := make_ident (String.concat ", "
  (List.map
   (function pt ->
     match (Ast_c.unwrap pt).Ast_c.p_namei with
       None -> failwith "bad param"
     | Some nm -> Ast_c.str_of_name nm)
    pl))

@@
identifier s.newargs;
identifier r.ff;
@@

  ff(
+ newargs
  )

@t@
identifier r.ff;
identifier x;
type T;
@@

ff(..., T *x, ...) { ... }

@@
identifier r.f, r.ff, t.x;
@@

f(...) {
++ assert(x != NULL);
   ...
   return ff(...);  // be sure this is the right function
} 

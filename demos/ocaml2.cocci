@r@
expression E;
statement S;
@@

if (f(E)) S

@script:ocaml@
(es,e) << r.E;
@@

(* note that an expression is a pair after being unwrapped *)
match Ast_c.unwrap e with
  (Ast_c.Ident nm,_) -> Printf.printf "argument %s is an identifier\n" es
| _ -> Printf.printf "argument %s is not an identifier\n" es

@script:ocaml@
(es,_) << r.E;
(ss,s) << r.S;
@@

(* note that a statement is not a pair after being unwrapped *)
match Ast_c.unwrap s with
  Ast_c.Jump _ ->
       Printf.printf "%s: branch %s is a jump\n" es ss
| _ -> Printf.printf "%s: branch %s is not a jump\n" es ss


@script:ocaml@
(ss,s) << r.S;
(es,_) << r.E;
@@

(* note that a statement is not a pair after being unwrapped *)
match Ast_c.unwrap s with
  Ast_c.Decl _ ->
       Printf.printf "%s: branch %s is a declaration\n" es ss
| _ -> Printf.printf "%s: branch %s is not a declaration\n" es ss

@script:ocaml@
(ss,s) << r.S;
es << r.E;
@@

(* note that a statement is not a pair after being unwrapped *)
match Ast_c.unwrap s with
  Ast_c.ExprStatement _ ->
       Printf.printf "%s: branch %s is an exprstatement\n" es ss
| _ -> Printf.printf "%s: branch %s is not an exprstatement\n" es ss

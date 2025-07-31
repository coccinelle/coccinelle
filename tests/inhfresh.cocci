@r1@
identifier oi;
//fresh identifier abc; //putting this in the parameter list below causes an error b/c abc unknown
fresh identifier fi = script:ocaml(oi) { (Printf.sprintf "%s_%s" "id" oi) };
@@
  int oi;
+ int fi;

@r2@
fresh identifier pi = script:ocaml(r1.fi) { (Printf.sprintf "%s_%s" fi "id") };
identifier r1.fi;
@@
- int fi;
+ int pi;

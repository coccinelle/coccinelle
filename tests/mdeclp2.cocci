@initialize:ocaml@
@@

let py2cocci_pos p =
  let ios = int_of_string in
  match Str.split (Str.regexp ",") p with
    [fl;fn;startl;startc;endl;endc] ->
      make_position fl fn (ios startl) (ios startc) (ios endl) (ios endc)
  | _ -> failwith "bad position"

@script:python a@
p;
@@

coccinelle.p = "tests/mdeclp2.c,one,1,4,1,7"

@script:ocaml r@
inp << a.p;
p;
@@

p := py2cocci_pos inp

@@
position r.p;
identifier f;
@@

- f@p(...) { ... }

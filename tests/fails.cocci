@initialize:ocaml@
@@

let ctr = ref 0

@@
@@

- kmalloc(...)
+ kzalloc(22)

@script:ocaml@
@@

ctr := !ctr + 1;
if !ctr = 3
then
  begin
    ctr := 0;
    failwith "fails"
  end

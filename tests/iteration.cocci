virtual after_start

@initialize:ocaml@
@@

let tbl = Hashtbl.create 100

@r@
identifier fn;
position p;
@@

fn@p(...) { <+... return (ERR(...)); ...+> }

@script:ocaml depends on r@
fn << r.fn;
p << r.p;
@@

try Hashtbl.find tbl fn
with Not_found ->
  Hashtbl.add tbl fn ();
  let it = new iteration () in
  it#set_files [(List.hd p).file];
  it#add_virtual_rule After_start;
  it#add_virtual_identifier Err_function fn;
  it#register ()

@e depends on after_start exists@
identifier virtual.err_function;
expression x;
position p;
@@

x@p = err_function(...)

@script:python@
p << e.p;
@@

cocci.print_main("call", p)

// run with the options -no_show_diff and -dir {directory}

virtual after_start

@initialize:ocaml@
@@

let tbl = Hashtbl.create(100)

let add_if_not_present from f file =
try let _ = Hashtbl.find tbl (f,file) in ()
with Not_found ->
   Hashtbl.add tbl (f,file) file;
   let it = new iteration() in
   (match file with
     Some fl -> it#set_files [fl]
   | None -> ());
   it#add_virtual_rule After_start;
   it#add_virtual_identifier Err_ptr_function f;
   it#register()

@r depends on !after_start exists@
identifier fn;
position p;
@@

fn@p(...) { <+... return (ERR_PTR(...)); ...+> }

@statfns@
identifier r.fn;
position r.p;
@@

static fn@p(...) { ... }

@script:ocaml depends on statfns@
fn << r.fn;
p << r.p;
@@

add_if_not_present "ERR_PTR" fn (Some ((List.hd p).file))

@script:ocaml depends on !statfns@
fn << r.fn;
p << r.p;
@@

add_if_not_present "ERR_PTR" fn None

// -----------------------------------------------------------------------
// iterate

@s depends on after_start exists@
identifier virtual.err_ptr_function, fn;
position p;
@@

fn@p(...) { <+... return err_ptr_function(...); ...+> }

@statfns_call@
identifier s.fn;
position s.p;
@@

static fn@p(...) { ... }

@script:ocaml depends on statfns_call@
fn << s.fn;
p << s.p;
err_ptr_function << virtual.err_ptr_function;
@@

add_if_not_present err_ptr_function fn (Some ((List.hd p).file))

@script:ocaml depends on !statfns_call@
fn << s.fn;
p << s.p;
err_ptr_function << virtual.err_ptr_function;
@@

add_if_not_present err_ptr_function fn None

// -----------------------------------------------------------------------
// find bugs

@e depends on after_start exists@
identifier virtual.err_ptr_function;
expression x;
identifier fld;
position p1,p2;
@@

(
IS_ERR(x = err_ptr_function(...))
|
x@p1 = err_ptr_function(...)
)
... when != IS_ERR(x)
(
 (IS_ERR(x) ||...)
|
x@p2->fld
)

@script:python@
p1 << e.p1;
p2 << e.p2;
@@

cocci.print_main("def",p1)
cocci.print_secs("ref",p2)

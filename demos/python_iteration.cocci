virtual after_start

@initialize:python@
@@

seen = set()

def add_if_not_present (source, f, file):
    if (f, file) not in seen:
        seen.add((f, file))
        it = Iteration()
        if file != None:
            it.set_files([file])
        it.add_virtual_rule(after_start)
        it.add_virtual_identifier(err_ptr_function, f)
        it.register()

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

@script:python depends on statfns@
fn << r.fn;
p << r.p;
@@

add_if_not_present("ERR_PTR", fn, p[0].file)

@script:python depends on !statfns@
fn << r.fn;
p << r.p;
@@

add_if_not_present("ERR_PTR", fn, None)

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

@script:python depends on statfns_call@
fn << s.fn;
p << s.p;
err_ptr_function << virtual.err_ptr_function;
@@

add_if_not_present(err_ptr_function, fn, p[0].file)

@script:python depends on !statfns_call@
fn << s.fn;
p << s.p;
err_ptr_function << virtual.err_ptr_function;
@@

add_if_not_present(err_ptr_function,fn, None)

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

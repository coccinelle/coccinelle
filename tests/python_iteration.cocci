virtual after_start

@initialize:python@
@@

seen = set()

@r@
identifier fn;
position p;
@@

fn@p(...) { <+... return (ERR(...)); ...+> }

@script:python depends on r@
fn << r.fn;
p << r.p;
@@

if fn not in seen:
    seen.add(fn)
    it = Iteration()
    it.set_files([p[0].file])
    it.add_virtual_rule(after_start)
    it.add_virtual_identifier(err_function, fn)
    it.register()

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

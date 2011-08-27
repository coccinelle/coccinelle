@anyid@
type t;
identifier id;
constant char [] cst;
fresh identifier new = id ## "_is_constant";
@@

t id = cst;
+t new;

@contains@
type t;
identifier anyid.id;
constant char [] anyid.cst =~ ".*FOO";
fresh identifier contains = id ##"_equals_cst_that_contains_FOO";
@@

t id = cst;
+t contains;

@nocontain@
type t;
identifier anyid.id;
constant char [] anyid.cst !~ ".*FOO";
fresh identifier nocontain = id ##"_equals_cst_that_doesn_t_contain_FOO";
@@

t id = cst;
+t nocontain;

@endsby@
type t;
identifier anyid.id;
constant char [] anyid.cst =~ ".*FOO$";
fresh identifier endsby = id ##"_equals_cst_that_ends_by_FOO";
@@

t id = cst;
+t endsby;

@beginsby@
type t;
identifier anyid.id;
constant char [] anyid.cst =~ "^FOO";
fresh identifier beginsby = id ##"_equals_cst_that_begins_by_FOO";
@@

t id = cst;
+t beginsby;

@format@
type t;
identifier anyid.id;
constant char [] anyid.cst =~ ".*%s";
fresh identifier strformat = id ##"_is_a_format_for_at_least_a_string";
@@

t id = cst;
+t strformat;


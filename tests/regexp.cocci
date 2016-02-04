@anyid@
type t;
identifier id;
fresh identifier new = id ## "_is_an_id";
@@

t id;
+t new;

@contains@
type t;
identifier anyid.id =~ "foo";
fresh identifier contains = id ##"_contains_foo";
@@

t id;
+t contains;

@nocontain@
type t;
identifier anyid.id !~ "foo";
fresh identifier nocontain = id ##"_doesn_t_contain_foo";
@@

t id;
+t nocontain;

@endsby@
type t;
identifier anyid.id =~ "foo$";
fresh identifier endsby = id ##"_ends_by_foo";
@@

t id;
+t endsby;

@beginsby@
type t;
identifier anyid.id =~ "^foo";
fresh identifier beginsby = id ##"_begins_by_foo";
@@

t id;
+t beginsby;

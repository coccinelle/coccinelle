@compound@
typedef uuid_t;
identifier os, is, ou, iu;
type outer = {struct os, union ou};
type inner = {struct is, union iu, uuid_t};
type t;
identifier x;
@@

    outer {
        ...
(
        inner x;
|
        inner x[...];
|
        t x[...];
|
	inner {
		...
	} x;
|
	inner {
		...
	} x[...];
)
        ...
    };

@single@
typedef uuid_t;
type t = {compound.outer, uuid_t};
identifier x;
@@

    t x = {
-       0
    };

@array_of_compound_type@
typedef uuid_t;
identifier s, u;
type t = {struct s, union u, uuid_t};
identifier x;
@@
    t x[...] = {
-       0
    };

@multi_dimensional_array_of_anything@
type t;
identifier x;
@@
    t x[...][...] = {
-       0
    };

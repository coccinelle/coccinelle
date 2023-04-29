@annotate@
type COUNTER_TYPE, ARRAY_TYPE;
identifier STRUCT;
identifier ARRAY;
identifier COUNTER;
attribute name __counted_by;
@@

 struct STRUCT {
        ...
        COUNTER_TYPE COUNTER;
        ...
        ARRAY_TYPE ARRAY[]
+       __counted_by(COUNTER)
        ;
 };

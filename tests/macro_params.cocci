@@
expression value;
expression list args;
identifier argc;
identifier func;
@@

func(..., int argc, ...) {
        <...
-       MACRO(value, args);
+       MACRO_2(argc, args);
        ...>
}


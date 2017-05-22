@@
@@

struct dvb_frontend {
        ...
-       struct dvb_frontend_ops* ops;
+       struct dvb_frontend_ops ops;
        ...
};

@@
identifier i, v;
type T;
@@

typedef struct {
        ...
-       u8 i : 1;
+       u8 i : 2;
        ...
} T;
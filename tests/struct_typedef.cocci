@@
@@

- struct dvb_frontend {
+ struct dvb2_frontend{
        ...
-       struct dvb_frontend_ops* ops;
+       struct dvb_frontend_ops ops;
        ...
};

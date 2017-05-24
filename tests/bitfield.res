struct dvb_frontend {
	struct dvb_frontend_ops ops;
};

typedef struct {
		u8 RESET:2;
		u8 IDLE:2;
		u8 STOP:2;
		u8 HIRQ0:2;
		u8 HIRQ1:2;
		u8 na0:2;
		u8 HABAV:2;
		u8 na1:2;

} bcm3510_register_value;

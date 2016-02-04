struct dvb_frontend {
	struct dvb_frontend_ops* ops;
};

typedef struct {
		u8 RESET :1;
		u8 IDLE  :1;
		u8 STOP  :1;
		u8 HIRQ0 :1;
		u8 HIRQ1 :1;
		u8 na0   :1;
		u8 HABAV :1;
		u8 na1   :1;

} bcm3510_register_value;

struct imx_sc_msg_req_set_clock_rate {
 	struct imx_sc_rpc_msg hdr;
 	__le32 rate;
 	__le16 resource;
 	u8 clk;
} __packed __aligned(4);

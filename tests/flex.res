struct scmi_msg_resp_sensor_description {
	__le16 num_remaining;
	struct {
#define SENSOR_UPDATE_BASE(x)	3
		    u8 name[SCMI_MAX_STR_SIZE];
	} desc[];
};

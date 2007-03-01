

void main(int i)
{

	request.message_id = MSG_SYSTEM_WAIT_SYNCHRO_CMD;
 	request.uid = (mixart_uid_t){0,0};
	request.data = &system_msg_uid;
	request.size = sizeof(system_msg_uid);


 	request.uid = (mixart_uid_t){0,0};  /* board num = 0 */


        *entry = (swp_entry_t) {0};

}



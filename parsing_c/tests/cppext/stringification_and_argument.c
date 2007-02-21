
void main(int i) {

  printk(KERN_INFO "scsi%d : at " STRVAL(NCR5380_map_name) " 0x%x", instance->host_no, (unsigned int) instance->NCR5380_instance_name);
}


MODULE_PARM_DESC(num_rcv_urbs,
		 "Number of urbs used for reception (range: 0-"
		 __MODULE_STRING(UDSL_MAX_RCV_URBS) ", default: "
		 __MODULE_STRING(UDSL_DEFAULT_RCV_URBS) ")");


MODULE_PARM (mtu, "1-" __MODULE_STRING (MAX_UNITS) "i");



void main(int i) {

}


MODULE_PARM(radio_nr, "i");

module_param(debug, bool, 0)

EXPORT_NO_SYMBOLS(1)
 
EXPORT_NO_SYMBOLS;



module_param(debug, bool, 0);
module_param(autoclose, bool, 0);


module_init(aztcd_init);
module_exit(aztcd_exit);

MODULE_LICENSE("GPL");
MODULE_ALIAS_BLOCKDEV_MAJOR(AZTECH_CDROM_MAJOR);

module_param(sony535_cd_base_io, int, 0);

__setup("sonycd535=", sonycd535_setup);

EXPORT_SYMBOL(mca_set_adapter_procfn);

MODULE_PARM_DESC(num_rcv_urbs,
		 "Number of urbs used for reception (range: 0-"
		 __MODULE_STRING(UDSL_MAX_RCV_URBS) ", default: "
		 __MODULE_STRING(UDSL_DEFAULT_RCV_URBS) ")");

module_param(debug, bool, 0)

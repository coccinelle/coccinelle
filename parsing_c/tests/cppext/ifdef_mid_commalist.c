
static const unsigned char sdtr_message[] = {
#ifdef CONFIG_SCSI_NCR53C7xx_FAST
    EXTENDED_MESSAGE, 3 /* length */, EXTENDED_SDTR, 25 /* *4ns */, 8 /* off */ 
#else
    EXTENDED_MESSAGE, 3 /* length */, EXTENDED_SDTR, 50 /* *4ns */, 8 /* off */ 
#endif
};

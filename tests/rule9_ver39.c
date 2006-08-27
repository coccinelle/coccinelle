static int eata_pio_proc_info(char *buffer, char **start, off_t offset,
			      int length, int hostno, int rw)
{
    struct Scsi_Host *shost;
    static u8 buff[512];
    int size, len = 0;
    off_t begin = 0, pos = 0;

    if (rw)
    	return -ENOSYS;
    shost = scsi_host_hn_get(hostno);
    if (!shost)
    	return -EINVAL;

    if (offset == 0)
	memset(buff, 0, sizeof(buff));

    size = sprintf(buffer+len, "EATA (Extended Attachment) PIO driver version: "
		   "%d.%d%s\n",VER_MAJOR, VER_MINOR, VER_SUB);
    len += size; pos = begin + len;
    size = sprintf(buffer + len, "queued commands:     %10ld\n"
		   "processed interrupts:%10ld\n", queue_counter, int_counter);
    len += size; pos = begin + len;
    
    size = sprintf(buffer + len, "\nscsi%-2d: HBA %.10s\n",
		   shost->host_no, SD(shost)->name);
    len += size; 
    pos = begin + len;
    size = sprintf(buffer + len, "Firmware revision: v%s\n", 
		   SD(shost)->revision);
    len += size;
    pos = begin + len;
    size = sprintf(buffer + len, "IO: PIO\n");
    len += size; 
    pos = begin + len;
    size = sprintf(buffer + len, "Base IO : %#.4x\n", (u32) shost->base);
    len += size; 
    pos = begin + len;
    size = sprintf(buffer + len, "Host Bus: %s\n", 
		   (SD(shost)->bustype == 'P')?"PCI ":
		   (SD(shost)->bustype == 'E')?"EISA":"ISA ");
    
    len += size; 
    pos = begin + len;
    
    if (pos < offset) {
	len = 0;
	begin = pos;
    }
    if (pos > offset + length)
	goto stop_output;
    
stop_output:
    DBG(DBG_PROC, printk("2pos: %ld offset: %ld len: %d\n", pos, offset, len));
    *start=buffer+(offset-begin);   /* Start of wanted data */
    len-=(offset-begin);            /* Start slop */
    if(len>length)
	len = length;               /* Ending slop */
    DBG(DBG_PROC, printk("3pos: %ld offset: %ld len: %d\n", pos, offset, len));
    
    return (len);     
}

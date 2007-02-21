// trailing ident
#endif MULTISESSION

void main(int i) { 
#if DEBUG_MULTIS
  if (ms.addr_format == CDROM_MSF)
    printk(KERN_DEBUG
           "optcd: multisession xa:%d, msf:%02d:%02d.%02d\n",
           ms.xa_flag,
           ms.addr.msf.minute,
           ms.addr.msf.second,
           ms.addr.msf.frame);
  else
    printk(KERN_DEBUG
           "optcd: multisession %d, lba:0x%08x [%02d:%02d.%02d])\n",
           ms.xa_flag,
           ms.addr.lba,
           disk_info.last_session.minute,
           disk_info.last_session.second,
           disk_info.last_session.frame);
#endif DEBUG_MULTIS
}

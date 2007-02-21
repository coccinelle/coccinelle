#if LINUX_VERSION_CODE < 0x20300
#define RQFUNC_ARG void
#else
#define RQFUNC_ARG request_queue_t *q
#endif

static void mtdblock_request(RQFUNC_ARG)
{
   struct request *current_request;
   unsigned int res = 0;
   struct mtd_info *mtd;
}

static void __init dc390_ReadEEprom(PDEVDECL, PUSHORT ptr)
{
     UCHAR   regval,cmd;
     UCHAR   i;
 
     cmd = EEPROM_READ;
     for(i=0; i<0x40; i++)
       {
         dc390_EnDisableCE(ENABLE_CE, PDEV, &regval);
         dc390_Prepare(PDEV, &regval, cmd++);
         *ptr++ = dc390_EEpromGetData1(PDEV);
         dc390_EnDisableCE(DISABLE_CE, PDEV, &regval);
       }
}



int __devinit acenic_probe (ACE_PROBE_ARG)
{
}


static int __init DC390_init (PSHT psht, ULONG io_port, UCHAR Irq, PDEVDECL, UCHAR index)
{
}
static void __init dc390_set_pci_cfg (PDEVDECL)
{
}


int __devinit acenic_probe (ACE_PROBE_ARG)
{

}

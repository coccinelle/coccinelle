

void main(int i) 
{
#if 0
	printk("53C400w: waiting for registers to be available\n");
	THEY NEVER DO ! while (NCR5380_read(C400_CONTROL_STATUS_REG) & CSR_53C80_REG);
	printk("53C400w: Got em\n");
#endif

}

void main(int i) 
{

#if 0
		xd_info[drive].rwrite = geometry_table[n][2];	/* reduced write */
		xd_info[drive].precomp = geometry_table[n][3]		/* write precomp */
		xd_info[drive].ecc = 0x0B;				/* ecc length */
#endif /* 0 */


}

void main(int i) 
{

//in linux-2.5.52/drivers/scsi/constants.c
int i = {
        {0x1, "Ram failure"},
#if 0
	{0x40NN, "Ram failure"},
	{0x40NN, "Diagnostic failure on component nn"},
	{0x41NN, "Data path failure"},
	{0x42NN, "Power-on or self-test failure"},
#endif
        {0x2, "Ram failure"},
 };



}

void main(int i) 
{


#if 0
 that then introduce some garbage
FIQ_VECTOR:    
	b HALT1
HALT1:	b HALT1
RESET_HANDLER:
	mov     r0, #CPSR_INITIAL
	msr	CPSR_c, r0	/* This is probably unnecessary */

#endif

}

typedef struct signature {
	const char *sig;	/* String to look for            */
	unsigned long ofs;	/* offset from BIOS base address */
	unsigned len;		/* length of string              */
} Signature;

static const Signature signatures[] = {
	{"SSTBIOS", 0x0000d, 7}	/* "SSTBIOS" @ offset 0x0000d */
};
#define NUM_SIGNATURES (sizeof(signatures)/sizeof(Signature))

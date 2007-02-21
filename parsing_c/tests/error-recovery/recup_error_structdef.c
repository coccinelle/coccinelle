typedef struct _xxx 
{
    'RXD'
    adv_sgblk_t          *adv_sgblkp;           /* Scatter-gather structures. */
    ushort               bios_signature;        /* BIOS Signature. */
    ushort               bios_version;          /* BIOS Version. */
    ushort               bios_codeseg;          /* BIOS Code Segment. */
    ushort               bios_codelen;          /* BIOS Code Segment Length. */
} asc_board_t;



typedef struct _PCI_DATA_
{
    uchar    type;
    uchar    bus;
    uchar    slot;
    uchar    func;
    uchar    offset;
} PCI_DATA;

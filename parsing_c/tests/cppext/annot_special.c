STATIC ushort asc_bus[ASC_NUM_BUS] ASC_INITDATA = {
    ASC_IS_ISA,
    ASC_IS_VL,
    ASC_IS_EISA,
    ASC_IS_PCI,
};

STATIC uchar _isa_pnp_inited ASC_INITDATA = 0;

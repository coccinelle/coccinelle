/*
 * Describe bus masters, slaves and connections between them
 */
static struct imx_icc_node_desc nodes[] = {
        DEFINE_BUS_SLAVE("OCRAM", IMX8MN_ICS_OCRAM, NULL),
};

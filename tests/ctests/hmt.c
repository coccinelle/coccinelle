int sa1100_mtd_init(void)
{
#ifdef CONFIG_SA1100_FRODO
        if (machine_is_frodo()) {
        }
#ifdef CONFIG_SA1100_GRAPHICSCLIENT
        if (machine_is_graphicsclient()) {
        }
#endif
}

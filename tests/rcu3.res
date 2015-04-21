static struct mtd_chip_driver *get_mtd_chip_driver (const char *name)
{
        struct list_head *pos;
        struct mtd_chip_driver *this;

        this = list_entry(_X(pos), typeof(*this), list);
        this = list_entry(_X(pos), struct foo, list);
}

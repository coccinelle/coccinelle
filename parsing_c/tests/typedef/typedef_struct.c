//HANDLING: /home/pad/kernels/src/linux-2.5.2/drivers/isdn/hisax/sedlbauer_cs.c
//semantic error two or more data types
// =File "/home/pad/kernels/src/linux-2.5.2/drivers/isdn/hisax/sedlbauer_cs.c", line 185, characters 14
//    around = ';', whole content = } local_info_t;
// charpos = 6958

typedef struct local_info_t {
    dev_link_t		link;
    dev_node_t		node;
    int			stop;
    struct bus_operations *bus;
} local_info_t;



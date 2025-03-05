#define soc_printk printk ("soc%d: ", s->soc_no); printk 


static inline void soc_init(struct sbus_dev *sdev, int no)
{
		soc_printk ("Cannot order irq %d to go\n", irq);
}

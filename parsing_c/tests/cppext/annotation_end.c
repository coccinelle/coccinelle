static unsigned int eisa_irq_level __read_mostly; 

void __iomem *eisa_eeprom_addr __read_mostly;

spinlock_t lock ____cacheline_aligned_in_smp;

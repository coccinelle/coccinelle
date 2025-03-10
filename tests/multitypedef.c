typedef struct HYSDN_CARD {
	struct work_struct irq_queue;
} hysdn_card;

int
ergo_inithardware(hysdn_card * card)
{
	INIT_WORK(&card->irq_queue, ergo_irq_bh, card);
}

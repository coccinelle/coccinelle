typedef	struct _st_cpc_tty_area {
	struct work_struct tty_tx_work; /* tx work - tx interrupt */
	struct work_struct tty_rx_work; /* rx work - rx interrupt */
	} st_cpc_tty_area;

void cpc_tty_init(pc300dev_t *pc300dev)
{
	unsigned long port;
	int aux;
	st_cpc_tty_area * cpc_tty;

	INIT_WORK(&cpc_tty->tty_tx_work, cpc_tty_tx_work);
}

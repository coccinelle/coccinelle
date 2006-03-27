/* fixed, a bug of linux ... */

static int aurora_open(struct tty_struct * tty, struct file * filp)
{
	int board;
	int error;
	struct Aurora_port * port;
	struct Aurora_board * bp;
	unsigned long flags;
	
#ifdef AURORA_DEBUG
	printk("aurora_open: start\n");
#endif
	
	board = AURORA_BOARD(tty->index);
	if (board > AURORA_NBOARD ||
	    !(aurora_board[board].flags & AURORA_BOARD_PRESENT)) {
#ifdef AURORA_DEBUG
		printk("aurora_open: error board %d present %d\n",
		       board, aurora_board[board].flags & AURORA_BOARD_PRESENT);
#endif
		return -ENODEV;
	}
	
	bp = &aurora_board[board];
	port = aurora_port + board * AURORA_NPORT * AURORA_NCD180 + AURORA_PORT(tty->index);
        if ((aurora_paranoia_check(port, tty->name, "aurora_open")) {         /*!!!!*/ 	
#ifdef AURORA_DEBUG
		printk("aurora_open: error paranoia check\n");
#endif
		return -ENODEV;
	}
            

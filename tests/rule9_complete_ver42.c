static int in2000_proc_info(char *buf, char **start, off_t off, int len, int hn, int in)
{

#ifdef PROC_INTERFACE

	char *bp;
	char tbuf[128];
	unsigned long flags;
	struct Scsi_Host *instance;
	struct IN2000_hostdata *hd;
	Scsi_Cmnd *cmd;
	int x, i;
	static int stop = 0;

	instance = scsi_host_hn_get(hn);
	if (!instance) {
		printk("*** Hmm... Can't find host #%d!\n", hn);
		return (-ESRCH);
	}
	hd = (struct IN2000_hostdata *) instance->hostdata;

/* If 'in' is TRUE we need to _read_ the proc file. We accept the following
 * keywords (same format as command-line, but only ONE per read):
 *    debug
 *    disconnect
 *    period
 *    resync
 *    proc
 */

	if (in) {
		buf[len] = '\0';
		bp = buf;
		if (!strncmp(bp, "debug:", 6)) {
			bp += 6;
			hd->args = simple_strtoul(bp, NULL, 0) & DB_MASK;
		} else if (!strncmp(bp, "disconnect:", 11)) {
			bp += 11;
			x = simple_strtoul(bp, NULL, 0);
			if (x < DIS_NEVER || x > DIS_ALWAYS)
				x = DIS_ADAPTIVE;
			hd->disconnect = x;
		} else if (!strncmp(bp, "period:", 7)) {
			bp += 7;
			x = simple_strtoul(bp, NULL, 0);
			hd->default_sx_per = sx_table[round_period((unsigned int) x)].period_ns;
		} else if (!strncmp(bp, "resync:", 7)) {
			bp += 7;
			x = simple_strtoul(bp, NULL, 0);
			for (i = 0; i < 7; i++)
				if (x & (1 << i))
					hd->sync_stat[i] = SS_UNSET;
		} else if (!strncmp(bp, "proc:", 5)) {
			bp += 5;
			hd->proc = simple_strtoul(bp, NULL, 0);
		} else if (!strncmp(bp, "level2:", 7)) {
			bp += 7;
			hd->level2 = simple_strtoul(bp, NULL, 0);
		}
		return len;
	}

	spin_lock_irqsave(instance->host_lock, flags);
	bp = buf;
	*bp = '\0';
	if (hd->proc & PR_VERSION) {
		sprintf(tbuf, "\nVersion %s - %s. Compiled %s %s", IN2000_VERSION, IN2000_DATE, __DATE__, __TIME__);
		strcat(bp, tbuf);
	}
	if (hd->proc & PR_INFO) {
		sprintf(tbuf, "\ndip_switch=%02x: irq=%d io=%02x floppy=%s sync/DOS5=%s", (hd->dip_switch & 0x7f), instance->irq, hd->io_base, (hd->dip_switch & 0x40) ? "Yes" : "No", (hd->dip_switch & 0x20) ? "Yes" : "No");
		strcat(bp, tbuf);
		strcat(bp, "\nsync_xfer[] =       ");
		for (x = 0; x < 7; x++) {
			sprintf(tbuf, "\t%02x", hd->sync_xfer[x]);
			strcat(bp, tbuf);
		}
		strcat(bp, "\nsync_stat[] =       ");
		for (x = 0; x < 7; x++) {
			sprintf(tbuf, "\t%02x", hd->sync_stat[x]);
			strcat(bp, tbuf);
		}
	}
#ifdef PROC_STATISTICS
	if (hd->proc & PR_STATISTICS) {
		strcat(bp, "\ncommands issued:    ");
		for (x = 0; x < 7; x++) {
			sprintf(tbuf, "\t%ld", hd->cmd_cnt[x]);
			strcat(bp, tbuf);
		}
		strcat(bp, "\ndisconnects allowed:");
		for (x = 0; x < 7; x++) {
			sprintf(tbuf, "\t%ld", hd->disc_allowed_cnt[x]);
			strcat(bp, tbuf);
		}
		strcat(bp, "\ndisconnects done:   ");
		for (x = 0; x < 7; x++) {
			sprintf(tbuf, "\t%ld", hd->disc_done_cnt[x]);
			strcat(bp, tbuf);
		}
		sprintf(tbuf, "\ninterrupts:      \t%ld", hd->int_cnt);
		strcat(bp, tbuf);
	}
#endif
	if (hd->proc & PR_CONNECTED) {
		strcat(bp, "\nconnected:     ");
		if (hd->connected) {
			cmd = (Scsi_Cmnd *) hd->connected;
			sprintf(tbuf, " %ld-%d:%d(%02x)", cmd->pid, cmd->device->id, cmd->device->lun, cmd->cmnd[0]);
			strcat(bp, tbuf);
		}
	}
	if (hd->proc & PR_INPUTQ) {
		strcat(bp, "\ninput_Q:       ");
		cmd = (Scsi_Cmnd *) hd->input_Q;
		while (cmd) {
			sprintf(tbuf, " %ld-%d:%d(%02x)", cmd->pid, cmd->device->id, cmd->device->lun, cmd->cmnd[0]);
			strcat(bp, tbuf);
			cmd = (Scsi_Cmnd *) cmd->host_scribble;
		}
	}
	if (hd->proc & PR_DISCQ) {
		strcat(bp, "\ndisconnected_Q:");
		cmd = (Scsi_Cmnd *) hd->disconnected_Q;
		while (cmd) {
			sprintf(tbuf, " %ld-%d:%d(%02x)", cmd->pid, cmd->device->id, cmd->device->lun, cmd->cmnd[0]);
			strcat(bp, tbuf);
			cmd = (Scsi_Cmnd *) cmd->host_scribble;
		}
	}
	if (hd->proc & PR_TEST) {
		;		/* insert your own custom function here */
	}
	strcat(bp, "\n");
	spin_unlock_irqrestore(instance->host_lock, flags);
	*start = buf;
	if (stop) {
		stop = 0;
		return 0;	/* return 0 to signal end-of-file */
	}
	if (off > 0x40000)	/* ALWAYS stop after 256k bytes have been read */
		stop = 1;;
	if (hd->proc & PR_STOP)	/* stop every other time */
		stop = 1;
	return strlen(bp);

#else				/* PROC_INTERFACE */

	return 0;

#endif				/* PROC_INTERFACE */

}

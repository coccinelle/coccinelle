
void main(int i)
{

	ft_failure = 0;
	TRACE_CATCH(fdc_init(),); /* init & detect fdc */


	TRACE_CATCH(ftape_report_raw_drive_status(&status),);


	m3_t *chip = snd_magic_cast(m3_t, dev_id, );

	snd_assert(ptr + chip->work_size == chip->work_ptr.area + chip->work_ptr.bytes, );


	wait_event_lock_irq(conf->wait_resume, !conf->barrier, conf->resync_lock, );


        snd_assert(dma->right_slot == chip->src_right_play_slot, );

}

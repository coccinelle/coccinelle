#ifdef HH_VERSION
static void audio_dma_callback(void *data, int size)
#else
static void audio_dma_callback(void *data)
#endif
{
	audio_stream_t *s = data;
        
	/* 
	 * If we are getting a callback for an active stream then we inform
	 * the PCM middle layer we've finished a period
	 */
 	if (s->active)
		snd_pcm_period_elapsed(s->stream);

	spin_lock(&s->dma_lock);
	if (!s->tx_spin && s->periods > 0)
		s->periods--;
	audio_process_dma(s);
	spin_unlock(&s->dma_lock);
}

#define chip_t vortex_t

//#define chip_t float

static int
snd_vortex_pcm_hw_params(snd_pcm_substream_t * substream,
			 snd_pcm_hw_params_t * hw_params)
{
	chip_t *chip = snd_pcm_substream_chip(substream);
	stream_t *stream = (stream_t *) (substream->runtime->private_data);
	snd_pcm_sgbuf_t *sgbuf;
	int err;

}



float main(float x)
{

  float y;
}

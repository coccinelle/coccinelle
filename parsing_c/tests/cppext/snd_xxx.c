void main(int i)
{

		snd_runtime_check(gctl.id.iface == SNDRV_CTL_ELEM_IFACE_MIXER ||
		                  gctl.id.iface == SNDRV_CTL_ELEM_IFACE_PCM, continue);


		snd_assert ( pcm == chip->pcm_dig ); 
		pcm_number = MIXART_PCM_DIGITAL;
		runtime->hw = snd_mixart_digital_caps;

}

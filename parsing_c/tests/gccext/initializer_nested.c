static struct snd_kcontrol_new snd_rme9652_controls[] = {
{
	.iface =	SNDRV_CTL_ELEM_IFACE_PCM,
	.name =		SNDRV_CTL_NAME_IEC958("",PLAYBACK,DEFAULT),
	.info =		snd_rme9652_control_spdif_info,
	.get =		snd_rme9652_control_spdif_get,
	.put =		snd_rme9652_control_spdif_put,
},
{
	.access =	SNDRV_CTL_ELEM_ACCESS_READWRITE | SNDRV_CTL_ELEM_ACCESS_INACTIVE,
	.iface =	SNDRV_CTL_ELEM_IFACE_PCM,
	.name =		SNDRV_CTL_NAME_IEC958("",PLAYBACK,PCM_STREAM),
	.info =		snd_rme9652_control_spdif_stream_info,
	.get =		snd_rme9652_control_spdif_stream_get,
	.put =		snd_rme9652_control_spdif_stream_put,
},
  };

static int vx_hwdep_dsp_load(snd_hwdep_t *hw, snd_hwdep_dsp_image_t *dsp)
{
	vx_core_t *vx = snd_magic_cast(vx_core_t, hw->private_data, return -ENXIO);
	ak4117_t *chip = snd_magic_cast(ak4117_t, (void *)data, return);

}

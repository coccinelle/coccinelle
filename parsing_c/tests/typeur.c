static int __devinit _snd_emu10k1_audigy_init_efx(emu10k1_t *emu)
{
	int err, i, z, gpr, nctl;
	const int playback = 10;
	const int capture = playback + (SND_EMU10K1_PLAYBACK_CHANNELS * 2); /* we reserve 10 voices */
	const int stereo_mix = capture + 2;
	const int tmp = 0x88;
	u32 ptr;
	emu10k1_fx8010_code_t *icode;
	emu10k1_fx8010_control_gpr_t *controls, *ctl;
	mm_segment_t seg;

	for (z = 0; z < 5; z++) {
		int j;
		for (j = 0; j < 2; j++) {
			controls[nctl + 0].gpr[z * 2 + j] = BASS_GPR + z * 2 + j;
			controls[nctl + 1].gpr[z * 2 + j] = TREBLE_GPR + z * 2 + j;
		}
	}
	for (z = 0; z < 4; z++) {		/* front/rear/center-lfe/side */
		int j, k, l, d;
		for (j = 0; j < 2; j++) {	/* left/right */
			k = 0xb0 + (z * 8) + (j * 4);
			l = 0xe0 + (z * 8) + (j * 4);
			d = playback + SND_EMU10K1_PLAYBACK_CHANNELS + z * 2 + j;

			if (z == 2)	/* center */
				break;
		}
	}
	nctl += 2;

	return err;
}


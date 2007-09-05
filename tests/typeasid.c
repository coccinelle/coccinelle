typedef struct snd_card_sa11xx_uda1341 {
	snd_card_t *card;
} sa11xx_uda1341_t;

snd_kcontrol_t *snd_ctl_new(snd_kcontrol_t * control, unsigned int access)
{
	sa11xx_uda1341_t *chip = xxx(sa11x_uda1341_t, card->pm_private_data);
}

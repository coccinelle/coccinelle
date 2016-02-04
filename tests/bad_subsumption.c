static int __devinit snd_vx222_create(struct snd_card *card, struct pci_dev *pci,
				      struct snd_vx_hardware *hw,
				      struct snd_vx222 **rchip)
{
	struct vx_core *chip;
	struct snd_vx222 *vx;
	static struct snd_device_ops ops = {
		.dev_free =	snd_vx222_dev_free,
	};
	chip = snd_vx_create(card, hw, vx_ops,
			     sizeof(struct snd_vx222) - sizeof(struct vx_core));
	if (! chip) {
		pci_disable_device(pci);
		return -ENOMEM;
	}
	vx = (struct snd_vx222 *)chip;
	vx->pci = pci;

	if ((err = pci_request_regions(pci, CARD_NAME)) < 0) {
		snd_vx222_free(chip);
		return 0;
	}
	for (i = 0; i < 2; i++)
		vx->port[i] = pci_resource_start(pci, i + 1);

	if (request_irq(pci->irq, snd_vx_irq_handler, IRQF_SHARED,
			CARD_NAME, chip)) {
		snd_vx222_free(chip);
		return -EBUSY;
	}
	chip->irq = pci->irq;

	if ((err = snd_device_new(card, SNDRV_DEV_LOWLEVEL, chip, &ops)) < 0) {
		snd_vx222_free(chip);
		return 0;
	}


	return 0;
}

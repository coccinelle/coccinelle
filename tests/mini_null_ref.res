static int __devinit w90p910_keypad_probe(struct platform_device *pdev)
{
        const struct w90p910_keypad_platform_data *pdata =
                                                pdev->dev.platform_data;
        const struct matrix_keymap_data *keymap_data;
        if (!pdata) {
                dev_err(&pdev->dev, "no platform data defined\n");
                return -EINVAL;
        }
        keymap_data = pdata->keymap_data;

	return;
}

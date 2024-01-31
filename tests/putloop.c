static int atmel_pctl_dt_node_to_map(struct pinctrl_dev *pctldev,
				     struct device_node *np_config,
				     struct pinctrl_map **map,
				     unsigned int *num_maps)
{
	if (ret) {
		for_each_child_of_node(np_config, np) {
			ret = atmel_pctl_dt_subnode_to_map(pctldev, np, map,
						    &reserved_maps, num_maps);
			if (ret < 0) {
				of_node_put(np);
				break;
			}
		}
	}

	foo();
	return ret;
}

void
voyager_cat_init(void)
{
	voyager_module_t **modpp;
	
	*modpp = kmalloc(sizeof(voyager_module_t), GFP_KERNEL);
	memset(*modpp, 0, sizeof(voyager_module_t));
}

struct info {
	const s16 (*rsv_chans)[2];
	int *buf;
};

int decl(struct info *info)
{
	const s16		(*reserved)[2];

	reserved = info->rsv_chans;
	info->buf = kmalloc_obj(*info->buf, GFP_KERNEL);
	return reserved != 0;
}

int cast(struct info *info, void *p)
{
	info->rsv_chans = (const s16 (*)[2])p;
	info->buf = kmalloc_obj(*info->buf, GFP_KERNEL);
	return 0;
}

int cast_nonconst(struct info *info, void *p)
{
	info->rsv_chans = (s16 (*)[2])p;
	info->buf = kmalloc_obj(*info->buf, GFP_KERNEL);
	return 0;
}

int expression(struct info *info, int *p)
{
	int a;

	a = lookup(*p)[1];
	info->buf = kmalloc_obj(*info->buf, GFP_KERNEL);
	return a;
}

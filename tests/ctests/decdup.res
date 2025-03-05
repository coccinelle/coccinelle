int main() {
	int x;
	int y, z;
	int z = 12;
	int a, b;

	return a;
}

static const struct seq_operations task_file_seq_ops = {
	.start = task_file_seq_start,
	.next = task_file_seq_next,
	.stop = task_file_seq_stop,
	.show = task_file_seq_show,
};

static int __init attr_event_init(void)
{
	struct attribute **attrs;
	int ret, i;

	attrs = kmalloc_array(ARRAY_SIZE(paiext_ctrnames) + 1, sizeof(*attrs),
			      GFP_KERNEL);
	if (!attrs)
		return -ENOMEM;
	for (i = 0; i < ARRAY_SIZE(paiext_ctrnames); i++) {
		ret = attr_event_init_one(attrs, i);
		if (ret) {
			attr_event_free(attrs, i - 1);
			return ret;
		}
	}
	attrs[i] = NULL;
	paiext_events_group.attrs = attrs;
	return 0;
}

static __init char *get_last_crashkernel(char *cmdline,
			     const char *name,
			     const char *suffix)
{
	char *p = cmdline, *ck_cmdline = NULL;

	/* find crashkernel and use the last one if there are more */
	p = strstr(p, name);
	while (p) {
		char *end_p = strchr(p, ' ');
		char *q;

		if (!end_p)
			end_p = p + strlen(p);

		if (!suffix) {
			int i;

			/* skip the one with any known suffix */
			for (i = 0; suffix_tbl[i]; i++) {
				q = end_p - strlen(suffix_tbl[i]);
				if (!strncmp(q, suffix_tbl[i],
					     strlen(suffix_tbl[i])))
					goto next;
			}
			ck_cmdline = p;
		} else {
			q = end_p - strlen(suffix);
			if (!strncmp(q, suffix, strlen(suffix)))
				ck_cmdline = p;
		}
next:
		p = strstr(p+1, name);
	}

	return ck_cmdline;
}

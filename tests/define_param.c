#define fieldsize(a)	(sizeof(a)/sizeof(*a))

static int
atari_proc_infos(unsigned char *nvram, char *buffer, int *len,
    off_t *begin, off_t offset, int size)
{
	for (i = fieldsize(boot_prefs) - 1; i >= 0; --i) {
		if (nvram[1] == boot_prefs[i].val) {
			PRINT_PROC("%s\n", boot_prefs[i].name);
			break;
		}
	}
}


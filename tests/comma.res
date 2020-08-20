static int
show_cpuinfo (struct seq_file *m, void *v)
{
	for (i = 0; i < ARRAY_SIZE(feature_bits) && size > 1; ++i) {
			cp += snprintf(cp, size, "%s%s", sep,
				       feature_bits[i].feature_name);
			cp += snprintf(cp, size, "%s%s", sep,
				       feature_bits[i].feature_name);
			sep = ", ";
	}
}

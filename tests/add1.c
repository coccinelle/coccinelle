static int create_scheduling_groups(void)
{
	struct cfs_ipa_sched_domain *sd;
	int i, ret;

	for (i = cfs_ipa_nr_topology_levels - 1; i > 0; i--) {
		ret = build_groups(sd, i);
	}

	list_for_each_entry(sd, cfs_ipa_topology, siblings) {
		ret = build_lower_groups(sd);
	}

	return 0;
}

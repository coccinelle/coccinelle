static int mlxsw_sp_flower_parse_actions(struct mlxsw_sp *mlxsw_sp,
					 struct mlxsw_sp_acl_block *block,
					 struct mlxsw_sp_acl_rule_info *rulei,
					 struct tcf_exts *exts,
					 struct netlink_ext_ack *extack)
{
	const struct tc_action *a;

	if (!tcf_exts_has_actions(exts))
		return 0;

	/* Count action is inserted first */
	err = mlxsw_sp_acl_rulei_act_count(mlxsw_sp, rulei, extack);
	if (err)
		return err;

	one();
	if (two())
		return 1;
	return 0;
}


static int mlxsw_sp_flower_parse_actions(struct mlxsw_sp *mlxsw_sp,
					 struct mlxsw_sp_acl_block *block,
					 struct mlxsw_sp_acl_rule_info *rulei,
					 struct tcf_exts *exts,
					 struct netlink_ext_ack *extack)
{
	const struct tc_action *a;

	if (!tcf_exts_has_actions(exts))
		return 0;

	/* Count action is inserted first */
	err = mlxsw_sp_acl_rulei_act_count(mlxsw_sp, rulei, extack);
	if (err)
		return err;

	if (is_tcf_gact_ok(a)) {
		err = mlxsw_sp_acl_rulei_act_terminate(rulei);
		if (err) {
			NL_SET_ERR_MSG_MOD(extack, "Cannot append terminate action");
			return err;
		}
	} else if (is_tcf_gact_shot(a)) {
		err = mlxsw_sp_acl_rulei_act_drop(rulei);
		if (err) {
			NL_SET_ERR_MSG_MOD(extack, "Cannot append drop action");
			return err;
		}
	} else if (is_tcf_gact_trap(a)) {
		err = mlxsw_sp_acl_rulei_act_trap(rulei);
		if (err) {
			NL_SET_ERR_MSG_MOD(extack, "Cannot append trap action");
			return err;
		}
	} else if (is_tcf_gact_goto_chain(a)) {
		u32 chain_index = tcf_gact_goto_chain_index(a);
		struct mlxsw_sp_acl_ruleset *ruleset;
		u16 group_id;

		ruleset = mlxsw_sp_acl_ruleset_lookup(mlxsw_sp, block,
						      chain_index,
						      MLXSW_SP_ACL_PROFILE_FLOWER);
		if (IS_ERR(ruleset))
			return PTR_ERR(ruleset);

		group_id = mlxsw_sp_acl_ruleset_group_id(ruleset);
		err = mlxsw_sp_acl_rulei_act_jump(rulei, group_id);
		if (err) {
			NL_SET_ERR_MSG_MOD(extack, "Cannot append jump action");
			return err;
		}
	} else if (is_tcf_mirred_egress_redirect(a)) {
		struct net_device *out_dev;
		struct mlxsw_sp_fid *fid;
		u16 fid_index;

		fid = mlxsw_sp_acl_dummy_fid(mlxsw_sp);
		fid_index = mlxsw_sp_fid_index(fid);
		err = mlxsw_sp_acl_rulei_act_fid_set(mlxsw_sp, rulei,
						     fid_index, extack);
		if (err)
			return err;

		out_dev = tcf_mirred_dev(a);
		err = mlxsw_sp_acl_rulei_act_fwd(mlxsw_sp, rulei,
						 out_dev, extack);
		if (err)
			return err;
} else if (is_tcf_mirred_egress_mirror(a)) {
		struct net_device *out_dev = tcf_mirred_dev(a);

		err = mlxsw_sp_acl_rulei_act_mirror(mlxsw_sp, rulei,
						    block, out_dev,
						    extack);
		if (err)
			return err;
} else if (is_tcf_vlan(a)) {
		u16 proto = be16_to_cpu(tcf_vlan_push_proto(a));
		u32 action = tcf_vlan_action(a);
		u8 prio = tcf_vlan_push_prio(a);
		u16 vid = tcf_vlan_push_vid(a);

		return mlxsw_sp_acl_rulei_act_vlan(mlxsw_sp, rulei,
						   action, vid,
						   proto, prio, extack);
	} else {
		NL_SET_ERR_MSG_MOD(extack, "Unsupported action");
		dev_err(mlxsw_sp->bus_info->dev, "Unsupported action\n");
		return -EOPNOTSUPP;
	}
	return 0;
}

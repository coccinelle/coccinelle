/**
 * ice_dump_common_caps - print struct ice_hw_common_caps fields
 * @hw: pointer to the ice_hw instance
 * @caps: pointer to common caps instance
 * @prefix: string to prefix when printing
 */
static void
ice_dump_common_caps(struct ice_hw *hw, struct ice_hw_common_caps *caps,
		     char const *prefix)
{
	dev_info(ice_hw_to_dev(hw), "%s: switching_mode = %d\n", prefix,
		 caps->switching_mode);
	dev_info(ice_hw_to_dev(hw), "%s: mgmt_mode = %d\n", prefix,
		 caps->mgmt_mode);
	dev_info(ice_hw_to_dev(hw), "%s: mgmt_protocols_mctp = %d\n", prefix,
		 caps->mgmt_protocols_mctp);
	dev_info(ice_hw_to_dev(hw), "%s: os2bmc = %d\n", prefix, caps->os2bmc);
	dev_info(ice_hw_to_dev(hw), "%s: valid_functions (bitmap) = %d\n",
		 prefix, caps->valid_functions);
	dev_info(ice_hw_to_dev(hw), "%s: sr_iov_1_1 = %d\n", prefix,
		 caps->sr_iov_1_1);
	dev_info(ice_hw_to_dev(hw), "%s: vmdq = %d\n", prefix, caps->vmdq);
	dev_info(ice_hw_to_dev(hw), "%s: evb_802_1_qbg = %d\n", prefix,
		 caps->evb_802_1_qbg);
	dev_info(ice_hw_to_dev(hw), "%s: evb_802_1_qbh = %d\n", prefix,
		 caps->evb_802_1_qbh);
	dev_info(ice_hw_to_dev(hw), "%s: dcb = %d\n", prefix, caps->dcb);
	dev_info(ice_hw_to_dev(hw), "%s: active_tc_bitmap = %d\n", prefix,
		 caps->active_tc_bitmap);
	dev_info(ice_hw_to_dev(hw), "%s: maxtc = %d\n", prefix, caps->maxtc);
	dev_info(ice_hw_to_dev(hw), "%s: iscsi = %d\n", prefix, caps->iscsi);
	dev_info(ice_hw_to_dev(hw), "%s: rss_table_size = %d\n", prefix,
		 caps->rss_table_size);
	dev_info(ice_hw_to_dev(hw), "%s: rss_table_entry_width = %d\n",
		 prefix, caps->rss_table_entry_width);
	dev_info(ice_hw_to_dev(hw), "%s: num_rxq = %d\n", prefix,
		 caps->num_rxq);
	dev_info(ice_hw_to_dev(hw), "%s: rxq_first_id = %d\n", prefix,
		 caps->rxq_first_id);
	dev_info(ice_hw_to_dev(hw), "%s: num_txq = %d\n", prefix,
		 caps->num_txq);
	dev_info(ice_hw_to_dev(hw), "%s: txq_first_id = %d\n", prefix,
		 caps->txq_first_id);
	dev_info(ice_hw_to_dev(hw), "%s: num_msix_vectors = %d\n", prefix,
		 caps->num_msix_vectors);
	dev_info(ice_hw_to_dev(hw), "%s: msix_vector_first_id = %d\n", prefix,
		 caps->msix_vector_first_id);
	dev_info(ice_hw_to_dev(hw), "%s: ieee_1588 = %d\n", prefix,
		 caps->ieee_1588);
	dev_info(ice_hw_to_dev(hw), "%s: mgmt_cem = %d\n", prefix,
		 caps->mgmt_cem);
	dev_info(ice_hw_to_dev(hw), "%s: iwarp = %d\n", prefix, caps->iwarp);
	dev_info(ice_hw_to_dev(hw), "%s: roce_lag = %d\n", prefix,
		 caps->roce_lag);
} /* This takes 27 seconds as of now, doubles with each ice_info() call added
	
	ice_info(hw, "%s: wr_csr_prot = 0x%llX\n", prefix,
		 (unsigned long long)caps->wr_csr_prot);
	ice_info(hw, "%s: num_wol_proxy_fltr = %d\n", prefix,
		 caps->num_wol_proxy_fltr);
	ice_info(hw, "%s: wol_proxy_vsi_seid = %d\n", prefix,
		 caps->wol_proxy_vsi_seid);
	ice_info(hw, "%s: max_mtu = %d\n", prefix, caps->max_mtu);
	ice_print_led_caps(hw, caps, prefix, false);
	ice_print_sdp_caps(hw, caps, prefix, false);
*/

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
	ice_info(hw, "%s: switching_mode = %d\n", prefix, caps->switching_mode);
	ice_info(hw, "%s: mgmt_mode = %d\n", prefix, caps->mgmt_mode);
	ice_info(hw, "%s: mgmt_protocols_mctp = %d\n", prefix,
		 caps->mgmt_protocols_mctp);
	ice_info(hw, "%s: os2bmc = %d\n", prefix, caps->os2bmc);
	ice_info(hw, "%s: valid_functions (bitmap) = %d\n", prefix,
		 caps->valid_functions);
	ice_info(hw, "%s: sr_iov_1_1 = %d\n", prefix, caps->sr_iov_1_1);
	ice_info(hw, "%s: vmdq = %d\n", prefix, caps->vmdq);
	ice_info(hw, "%s: evb_802_1_qbg = %d\n", prefix, caps->evb_802_1_qbg);
	ice_info(hw, "%s: evb_802_1_qbh = %d\n", prefix, caps->evb_802_1_qbh);
	ice_info(hw, "%s: dcb = %d\n", prefix, caps->dcb);
	ice_info(hw, "%s: active_tc_bitmap = %d\n", prefix,
		 caps->active_tc_bitmap);
	ice_info(hw, "%s: maxtc = %d\n", prefix, caps->maxtc);
	ice_info(hw, "%s: iscsi = %d\n", prefix, caps->iscsi);
	ice_info(hw, "%s: rss_table_size = %d\n", prefix, caps->rss_table_size);
	ice_info(hw, "%s: rss_table_entry_width = %d\n", prefix,
		 caps->rss_table_entry_width);
	ice_info(hw, "%s: num_rxq = %d\n", prefix, caps->num_rxq);
	ice_info(hw, "%s: rxq_first_id = %d\n", prefix, caps->rxq_first_id);
	ice_info(hw, "%s: num_txq = %d\n", prefix, caps->num_txq);
	ice_info(hw, "%s: txq_first_id = %d\n", prefix, caps->txq_first_id);
	ice_info(hw, "%s: num_msix_vectors = %d\n", prefix,
		 caps->num_msix_vectors);
	ice_info(hw, "%s: msix_vector_first_id = %d\n", prefix,
		 caps->msix_vector_first_id);
	ice_info(hw, "%s: ieee_1588 = %d\n", prefix, caps->ieee_1588);
	ice_info(hw, "%s: mgmt_cem = %d\n", prefix, caps->mgmt_cem);
	ice_info(hw, "%s: iwarp = %d\n", prefix, caps->iwarp);
	ice_info(hw, "%s: roce_lag = %d\n", prefix, caps->roce_lag);
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

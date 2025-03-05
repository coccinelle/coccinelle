static efi_status_t virt_efi_get_time(efi_time_t *tm, efi_time_cap_t *tc)
{
	status = ({

	if (!efi_enabled(EFI_RUNTIME_SERVICES))
		goto exit;

exit:
	efi_rts_work.efi_rts_id = 12;
});
}

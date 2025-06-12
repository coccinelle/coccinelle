int main() {
	dat_w0 = dat_w0_read(dat_idx);
	FIELD_MODIFY(DAT_0_STATIC_ADDRESS, &dat_w0, address);
	dat_w0_write(dat_idx, dat_w0);
}

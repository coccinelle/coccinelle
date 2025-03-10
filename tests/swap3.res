static void __ar955x_tx_iq_cal_sort(struct ath_hw *ah,
				    struct coeff *coeff,
				    int i, int nmeasurement)
{
	int im, ix, iy;

	for (iy = ix + 1; iy <= MAXIQCAL - 1; iy++) {
//	if ( iy <= MAXIQCAL - 1) {
	  if (coeff->mag_coeff[i][im][iy] <
	      coeff->mag_coeff[i][im][ix]) {
	    swap(coeff->mag_coeff[i][im][ix],
		 coeff->mag_coeff[i][im][iy],
		 temp);
	  }
	}
}


struct ncb {
	u_char	sv_scntl0, sv_scntl3, sv_dmode, sv_dcntl, sv_ctest3, sv_ctest4,
		sv_ctest5, sv_gpcntl, sv_stest2, sv_stest4, sv_stest1, sv_scntl4;

	/*----------------------------------------------------------------
	**	Actual initial value of IO register bits used by the 
	**	driver. They are loaded at initialisation according to  
	**	features that are to be enabled.
	**----------------------------------------------------------------
	*/
	u_char	rv_scntl0, rv_scntl3, rv_dmode, rv_dcntl, rv_ctest3, rv_ctest4, 
		rv_ctest5, rv_stest2, rv_ccntl0, rv_ccntl1, rv_scntl4;

};

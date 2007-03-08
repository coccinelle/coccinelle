extern __declspec(dllimport) u08bits ScsiPortReadPortUchar(pu08bits ioport);

static void finish_request(
	struct sl811		*sl811,
	struct sl811h_ep	*ep,
	struct urb		*urb,
	struct pt_regs		*regs,
	int			status
) __releases(sl811->lock) __acquires(sl811->lock)
{
}

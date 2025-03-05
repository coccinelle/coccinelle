#define CS_CHECK(fn, ret) \
do { last_fn = (fn); if ((last_ret = (ret)) != 0) goto cs_failed; } while (0)

static void nsp_cs_config(dev_link_t *link)
{
	client_handle_t	  handle = link->handle;
	CS_CHECK(GetFirstTuple, pcmcia_get_first_tuple(handle, &tuple));
cs_failed:
	return;
}

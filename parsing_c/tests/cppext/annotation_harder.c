static void finish_request(struct isp116x *isp116x, struct isp116x_ep *ep,
			   struct urb *urb, struct pt_regs *regs)
__releases(isp116x->lock) __acquires(isp116x->lock)
{
}


static void finish_request(struct isp116x *isp116x, struct isp116x_ep *ep,
			   struct urb *urb, struct pt_regs *regs)
     __releases(isp116x->lock) __acquires(isp116x->lock);

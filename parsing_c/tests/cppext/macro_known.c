void main(int i) 
{

	cb = (struct irda_skb_cb *) skb->cb;
 	ASSERT(cb != NULL, return;);
	self = (struct irda_usb_cb *) cb->context;
	ASSERT(self != NULL, return;);

}


ASC_INITFUNC(
void,
advansys_setup(char *str, int *ints)
)
{
    int    i;
}

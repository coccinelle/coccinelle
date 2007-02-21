
void main(int i)
{

  int i = 
#ifdef CONFIG_MODULE_UNLOAD
		      (p_drv->owner) ? module_refcount(p_drv->owner) : 1
#else
		      1
#endif
    ;

}




void main(int i)
{


#ifdef NORMAL_MEM_ACCESS
	if ((MEM->tx_head[(entry+1) & TX_RING_MOD_MASK].flag & TMD1_OWN) ==
#else
	if ((GET_FLAG(&MEM->tx_head[(entry+1) & TX_RING_MOD_MASK]) & TMD1_OWN) ==
#endif
            1)
            foo();

}

void main(int i)
{


            f(            
#ifdef __sparc__
		__irq_itoa(device->slot.irq));
#else
		device->slot.irq);
#endif


}

void main(int i)
{
  f(

#ifdef __sparc__
		__irq_itoa(np->irq));
#else
		(int) np->irq);
#endif
}


void main(int i)
{


        if( (foo(
#ifdef CS4231
							1, 2)) < 0) {
#else
							0, 1)) < 0) {
#endif	/* CS4231 */
       }


}

void main(int i)
{


    foo(
#ifndef CONFIG_SCC_TRXECHO
			    scc->kiss.softdcd)
#else
			    1)
#endif
     ;
}

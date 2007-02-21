void main(int i)
{


            for( i = 0; i < RX_RING_SIZE; i++ )
#ifdef NORMAL_MEM_ACCESS
                        if (MEM->rx_head[(entry+i) & RX_RING_MOD_MASK].flag &
#else
						if (GET_FLAG(&MEM->rx_head[(entry+i) & \
												  RX_RING_MOD_MASK]) &
#endif
                                                    1)
                            foo();
}                            

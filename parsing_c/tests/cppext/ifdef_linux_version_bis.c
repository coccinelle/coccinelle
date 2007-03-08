
void main(int i) 
{

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,20) && \
    LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
                 shp->highmem_io  = 0;
#endif

}

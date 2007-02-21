
void main(int i)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,75)
		extern char x86;
		switch(x86) {
#else
		switch(boot_cpu_data.x86) {
#endif

                }
}

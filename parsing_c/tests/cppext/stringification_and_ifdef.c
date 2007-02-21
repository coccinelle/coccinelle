void main(int i)
{

	copy_info(&info, "  On PCI bus %d, device %d, function %d, "
#ifdef __sparc__
		"IRQ %s\n",
#else
		"IRQ %d\n",
#endif

                  1);
}

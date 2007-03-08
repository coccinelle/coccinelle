void main(int i)
{
	asm("mrc%? p15, 0, %0, c0, c0" : "=r" (chiprev));

}

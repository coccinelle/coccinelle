void main(int i)
{

	kzalloc(2 * sizeof(int), GFP_KERNEL);
	kzalloc(sizeof(int) * 2, GFP_KERNEL);

}

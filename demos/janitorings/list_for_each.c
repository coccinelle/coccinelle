void main(int i)
{
	struct list_head *list;

	for (list = ymf_devs.next; list != &ymf_devs; list = list->next) {
		printf("cava");
	}

}

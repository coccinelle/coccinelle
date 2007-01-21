@@
expression E;
@@

- page_to_pfn(vmalloc_to_page(E))
+ vmalloc_to_pfn(E)

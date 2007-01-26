@@
identifier adr, kva, ret, kvirt_to_pa_fn;
@@

- kvirt_to_pa_fn(unsigned long adr) {
-       ...
-       kva = (unsigned long) page_address(vmalloc_to_page((void *)adr));
- 	kva |= adr & (PAGE_SIZE-1);
- 	ret = __pa(kva);
-       return ret;
- }

@@
identifier page;
expression pos;
expression A, B, C, D;
@@

- page = kvirt_to_pa_fn(pos);
+ page = page_to_pfn(vmalloc_to_page((void *)pos));
...
- remap_page_range(A, B, page, C, D)
+ remap_pfn_range(A, B, page, C, D)

@@
identifier page;
expression pos;
expression A, B, C, D;
@@

- page = kvirt_to_pa(pos);
+ page = page_to_pfn(vmalloc_to_page((void *)pos));
...
- remap_page_range(A, B, page, C, D)
+ remap_pfn_range(A, B, page, C, D)

// the following are a rule and its specializations to the case where C or
// X is 0.  I have no idea if this is what is meant.
@@
expression A, B, C, D, E, X;
@@

- remap_page_range(A,B,C<<PAGE_SHIFT+X,D,E)
+ remap_pfn_range(A,B,C+(X>>PAGE_SHIFT),D,E)

@@
expression A, B, C, D, E;
@@

- remap_page_range(A,B,C<<PAGE_SHIFT,D,E)
+ remap_pfn_range(A,B,C,D,E)

@@
expression A, B, D, E, X;
@@

- remap_page_range(A,B,X,D,E)
+ remap_pfn_range(A,B,X>>PAGE_SHIFT,D,E)

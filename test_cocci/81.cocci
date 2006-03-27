@@
expression E1, E2;
expression A, B, C, D;
expression P, Q, R, S, T, U;
@@

(
- E1 = kvirt_to_pa(E2);
- remap_page_range(A, B, E1, C, D);
+ E1 = vmalloc_to_pfn((void *)E2);
+ remap_pfn_range(A, B, E1, C, D);
|
- remap_page_range(P, Q, R<<PAGE_SHIFT+S, T, U)
+ remap_pfn_range(P, Q, R+(S>>PAGE_SHIFT), T, U)
)

@@
struct page *page;
@@

(
- LockPage(page)
+ SetPageLocked(page)
|
- UnlockPage(page)
+ unlock_page(page)
|
- Page_Uptodate(page)
+ PageUptodate(page)
|
- wait_on_page(page)
+ wait_on_page_locked(page)
)

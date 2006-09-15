//----------------------------------------------------------------------
// Rule 42
//----------------------------------------------------------------------

@@
expression page;
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
- TryLockPage(page)
+ TestSetPageLocked(page)
|
- PageClearSlab(page)
+ ClearPageSlab(page)
|
- PageSetSlab(page)
+ SetPageSlab(page)
|
- __SetPageReserved(page)
+ SetPageReserved(page)

)

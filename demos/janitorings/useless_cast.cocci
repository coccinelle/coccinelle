@@
type T;
T E;
@@

-     (T)
      E

// // special case
//@@
//type T;
//identifier x;
//T E;
//@@
//
//T x = 
//-     (T)
//      E;


// // kmalloc for instance don't require cast.
//@@
//type T;
//identifier x;
//void *E;
//@@
//
//T x = 
//-     (T)
//      E;


// kmalloc, etc
// __get_free_page
// get_zeroed_page
// RING_GET_RESPONSE
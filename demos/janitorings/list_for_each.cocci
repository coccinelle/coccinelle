@@
iterator list_for_each;
statement S;
expression head;
struct list_head *pos;
@@
- for (pos = head.next; pos != &head; pos = pos->next) 
-  S
//+ list_for_each(pos, &head) 


@@
//iterator list_for_each;
statement S;
expression head;
struct list_head *pos;
@@
- for (pos = head->next; pos != head; pos = pos->next) 
-  S
//+ list_for_each(pos, head) 



// list_add, list_del
// list_move
// list_del_init
// list_add_tail

// { ... when != 
//    list_del() \| xxx->next = E \| list_add() \| f(...,xxx,...)
// }


// le cas single statement



// in  0c5719c43d34073f6b4b0a2dd99f5317a5f63abd
//
//-    struct list_head *walk = &pdev->bus_list;
//+    struct list_head *walk;
//
//-    for (walk = walk->next; walk != &pdev->bus_list; walk = walk->next) {

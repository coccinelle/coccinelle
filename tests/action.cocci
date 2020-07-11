@replacement@
identifier action, work;
expression list el;
@@
 void work(...)
 {
 <+...
 if (...)
-{
-   action(el);
-   return;
-}
+   goto last_action;
 ...+>
+last_action:
 action(el);
 }

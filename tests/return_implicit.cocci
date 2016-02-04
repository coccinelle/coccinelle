@@
identifier fn;
@@

fn(...) 
{
    foo(...);
    ...
-   return;
+   return -ENODEV;
}
@@
expression node, child;
symbol drop_me;
iterator name for_each_child_of_node;
@@

for_each_child_of_node(node,child) {
  ...
+ of_node_put(drop_me, child);
}

@@
expression node, child;
symbol drop_me;
iterator name for_each_child_of_node, for_each_child_of_node_scoped;
statement S;
identifier L;
@@

-for_each_child_of_node
+for_each_child_of_node_scoped
  (node,child) {
   ... when strict
(
-   {
-   of_node_put(child);
    return ...;
-   }
|
-   {
-   of_node_put(child);
    goto L;
-   }
|
-   {
-   of_node_put(child);
    break;
-   }
|
-   of_node_put(child);
    return ...;
|
-   of_node_put(child);
    break;
|
-  of_node_put(drop_me, child);
)
}

@@
expression child;
symbol drop_me;
@@

- of_node_put(drop_me, child);

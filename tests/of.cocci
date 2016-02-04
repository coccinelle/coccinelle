@r@
identifier d;
expression e;
@@

for (d = NULL; (d = of_find_all_nodes(d)); )
 {... 
?  return e;
+  of_node_put(d);
...}

@r1@
identifier d;
expression e;
@@

for (d = NULL; (d = something(d)); )
 {... 
+  of_node_put(d);
?  return e;
...}

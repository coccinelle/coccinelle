@ rule1 @ type T; identifier I; constant C; expression E; @@
T I[C];
<... 
*I[E]
...>
@ rule2 @ type rule1.T; @@
* T

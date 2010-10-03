@@ 
expression e1, e2; 
@@

- if(check_region(e1,e2)!=0)
+ if(!request_region(e1,e2))
  { ... return ...; }
  <...
+ release_region(e1);
  return ...;
  ...>
- request_region(e1,e2);



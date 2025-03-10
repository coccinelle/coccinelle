@exists@
expression e,e1;
@@

if (e) {
  ... when forall
      when != e1
- return e1;
}
return 15;

@@
@@

- 0
+ 100

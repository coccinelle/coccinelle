// script on KJ:
// You can probably find examples of that by running:
// 
// $ grep -Er -B10 "memset ?\(.*,0 ?," * | less
// 
// and searching for the string "kmalloc" in the output.




// have to duplicate a lot of rules because T E only matches if E has a known
// type, even if T is not used elsewhere.

// originally, the whens were when != x, but that doesn't work because x
// only binds to the outermost expression, not all possible expressions, and
// the value returned by kmalloc is usually used as a subexpression.
// so we have considered the typical uses that may cause problems; a function
// call or dereference

//\(x->fld\|f(...,x,...)\|x=E\)

@@
type T2;
expression x;
identifier f,fld;
expression E;
expression E1,E2;
expression e1,e2,e3,y;
statement S;
@@

 x = 
- kmalloc
+ kzalloc
  (E1,E2)
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
- memset((T2)x,0,E1);

@@
type T, T2;
type T1;
T1 *x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S;
@@

 x = 
- kmalloc
+ kzalloc
 (sizeof(T1),E2)
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
- memset((T2)x,0,sizeof(*x));

@@
type T, T2;
type T1;
T1 *x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S;
@@

 x = 
- kmalloc
+ kzalloc
 (sizeof(*x),E2)
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
- memset((T2)x,0,sizeof(T1));

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
@@
type T, T2;
expression x;
identifier f,fld;
expression E;
expression E1,E2;
expression e1,e2,e3,y;
statement S, S1;
@@

 x = 
- kmalloc
+ kzalloc
  (E1,E2)
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
  if(x != NULL) {
     ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
-    memset((T2)x,0,E1);
     ...
  } else S1

@@
type T, T2;
type T1;
T1 *x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S, S1;
@@

 x = 
- kmalloc
+ kzalloc
 (sizeof(T1),E2)
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
  if(x != NULL) {
     ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
-    memset((T2)x,0,sizeof(*x));
     ...
  } else S1

@@
type T, T2;
type T1;
T1 *x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S, S1;
@@

 x = 
- kmalloc
+ kzalloc
  (sizeof(*x),E2)
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
  if(x != NULL) {
    ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
-   memset((T2)x,0,sizeof(T1));
     ...
  } else S1

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
@@
type T, T2;
type T1;
identifier x;
identifier f,fld;
expression E;
expression E1,E2;
expression e1,e2,e3,y;
statement S;
@@

T1 x = 
- kmalloc
+ kzalloc
 (E1,E2);
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
- memset((T2)x,0,E1);

@@
type T, T2;
type T1;
identifier x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S;
@@

T1 x = 
- kmalloc
+ kzalloc
  (sizeof(T1),E2);
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
- memset((T2)x,0,sizeof(*x));

@@
type T, T2;
type T1;
identifier x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S;
@@

T1 x = 
- kmalloc
+ kzalloc
 (sizeof(*x),E2);
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
- memset((T2)x,0,sizeof(T1));

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
@@
type T, T2;
type T1;
identifier x;
identifier f,fld;
expression E;
expression E1,E2;
expression e1,e2,e3,y;
statement S, S1;
@@

T1 x = 
- kmalloc
+ kzalloc
 (E1,E2);
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
  if(x != NULL) {
     ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
-    memset((T2)x,0,E1);
     ...
  } else S1

@@
type T, T2;
type T1;
identifier x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S, S1;
@@

T1 x = 
- kmalloc
+ kzalloc
  (sizeof(T1),E2);
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
  if(x != NULL) {
     ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
-    memset((T2)x,0,sizeof(*x));
     ...
  } else S1

@@
type T, T2;
type T1;
identifier x;
identifier f,fld;
expression E;
expression E2;
expression e1,e2,e3,y;
statement S, S1;
@@

T1 x = 
- kmalloc
+ kzalloc
 (sizeof(*x),E2);
  ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
  if(x != NULL) {
     ...  when != \(x->fld=E;\|y=f(...,x,...);\|f(...,x,...);\|x=E;\|while(...) S\|for(e1;e2;e3) S\)
-    memset((T2)x,0,sizeof(T1));
     ...
  } else S1

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
@@
expression E1,E2,E3;
@@

- kzalloc(E1 * E2,E3)
+ kcalloc(E1,E2,E3)

@@
{struct SHT, SHT_t} fops;
//struct SHT fops; // this one works
identifier proc_info_func;
@@
 fops.proc_info = proc_info_func;


@@
@@

- proc_info_func
+ foobar


// necessary :( because previous rule is a Exp and funheader 
// is not an expression.
@@
@@
- proc_info_func
+ foobar
  (...) 
{ 
  ...
}
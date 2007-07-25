// Thomas Surrel version. Error msg is "not handling multiple minirules" 
// @@
// identifier func;
// @@
// - int __init func(void)
//  { ... }
// + static int __init func(void)
//  { ... }

// cant match on __init in cocci_vs_c
//@@
//identifier func;
//@@
// - int __init func(void)
// + static int __init func(void)
//  { ... }


// cant parse
//@@
//identifier func;
//@@
// (
//  static int func(void) 
//  { ... }
// |
// + static 
//   int func(void) 
//  { ... }
// )


// wrong unparsing
//@@
//identifier initfunc;
//@@
//
//+ static 
//  int initfunc(void)
//  { ... }


//@ rule1 @
// identifier initfunc;
// @@
// module_init(initfunc);
// 
// @ rule2 @
// identifier rule1.initfunc;
// @@
// 
// - int 
// + static int 
//   initfunc(void)
//   { ... }



@ rule1 @
identifier initfunc;
@@
 module_init(initfunc);
 
@ rule2 @
identifier rule1.initfunc;
@@

static int initfunc(...)
 { ... }

@ rule3 depends on !rule2 @
identifier rule1.initfunc;
@@

//- int initfunc(void)
//+ static int  initfunc(void)
//   { ... }
 
- int 
+ static int 
   initfunc(...)
   { ... }

// Thomas Surrel version 
// @@
// identifier func;
// @@
// - int __init func(void)
//  { ... }
// + static int __init func(void)
//  { ... }


@@
identifier func;
@@
// - int __init func(void)
// + static int __init func(void)
//  { ... }

// (
//  static int func(void) 
//  { ... }
// |
// + static 
//   int func(void) 
//  { ... }
// )

+ static 
  int func(void)
  { ... }

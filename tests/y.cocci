@@
//local function interrupt;
identifier interrupt, cs;
@@

interrupt(...) {
  ...
- if (!cs) { ... return; }
  ...
}

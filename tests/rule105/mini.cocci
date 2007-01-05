@@
identifier link, f, handle_var;
expression E1, E2;
@@

f(...,dev_link_t *link,...) {
  client_handle_t handle_var;
  <...
- cs_error(handle_var,E1,E2)
+ cs_error(link,E1,E2)
  ...>
}

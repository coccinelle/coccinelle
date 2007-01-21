@@
identifier f, p_dev, link;
@@

// transformation.ml doesn't allow transforming inside a parameter when
// there are ...s, so have to hope for the best
f(struct pcmcia_device *
-                       p_dev
+                       link
   ) {
  ...
- dev_link_t *link = dev_to_instance(p_dev);
  <...
- p_dev
+ link
  ...>
}

@@
identifier link, f, handle_var;
expression E1, E2;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- first_tuple(handle_var,E1,E2)
+ first_tuple(link,E1,E2)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_request_io(handle_var,E1)
+ pcmcia_request_io(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_request_irq(handle_var,E1)
+ pcmcia_request_irq(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_request_configuration(handle_var,E1)
+ pcmcia_request_configuration(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_access_configuration_register(handle_var,E1)
+ pcmcia_access_configuration_register(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_disable_device(handle_var)
+ pcmcia_disable_device(link)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_get_first_tuple(handle_var,E1)
+ pcmcia_get_first_tuple(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_get_next_tuple(handle_var,E1)
+ pcmcia_get_next_tuple(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
expression E1;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_get_tuple_data(handle_var,E1)
+ pcmcia_get_tuple_data(link,E1)
  ...>
}

@@
identifier link, f, handle_var;
expression E1, E2;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- pcmcia_parse_tuple(handle_var,E1,E2)
+ pcmcia_parse_tuple(link,E1,E2)
  ...>
}

@@
identifier link, f, handle_var;
expression E1, E2;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- cs_error(handle_var,E1,E2)
+ cs_error(link,E1,E2)
  ...>
}

@@
identifier link, f, handle_var;
@@

f(...,dev_link_t *link,...) {
  ...
(
  client_handle_t handle_var = link->handle;
|
  struct pcmcia_device *handle_var = link->handle;
|
  client_handle_t handle_var;
  ...
  handle_var = link->handle;
|
  struct pcmcia_device *handle_var;
  ...
  handle_var = link->handle;
)
  <...
- handle_to_dev(handle_var)
+ handle_to_dev(link)
  ...>
}


@@
identifier link, f, handle_var;
@@

f(...,dev_link_t *link,...) {
  ...
(
- client_handle_t handle_var = link->handle;
|
- struct pcmcia_device *handle_var = link->handle;
|
- client_handle_t handle_var;
  ...
- handle_var = link->handle;
|
- struct pcmcia_device *handle_var;
  ...
- handle_var = link->handle;
)
  ... when != handle_var
}

@@
dev_link_t *link;
expression E1, E2;
@@

- first_tuple(link->handle,E1,E2)
+ first_tuple(link,E1,E2)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_request_io(link->handle,E1)
+ pcmcia_request_io(link,E1)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_request_irq(link->handle,E1)
+ pcmcia_request_irq(link,E1)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_request_configuration(link->handle,E1)
+ pcmcia_request_configuration(link,E1)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_access_configuration_register(link->handle,E1)
+ pcmcia_access_configuration_register(link,E1)

@@
dev_link_t *link;
expression E1, E2;
@@

- pcmcia_request_window(&link->handle,E1,E2)
+ pcmcia_request_window(&link,E1,E2)

@@
dev_link_t *link;
@@

- pcmcia_disable_device(link->handle)
+ pcmcia_disable_device(link)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_get_first_tuple(link->handle,E1)
+ pcmcia_get_first_tuple(link,E1)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_get_next_tuple(link->handle,E1)
+ pcmcia_get_next_tuple(link,E1)

@@
dev_link_t *link;
expression E1;
@@

- pcmcia_get_tuple_data(link->handle,E1)
+ pcmcia_get_tuple_data(link,E1)

@@
dev_link_t *link;
expression E1, E2;
@@

- pcmcia_parse_tuple(link->handle,E1,E2)
+ pcmcia_parse_tuple(link,E1,E2)

@@
dev_link_t *link;
expression E1, E2;
@@

- cs_error(link->handle,E1,E2)
+ cs_error(link,E1,E2)

@@
dev_link_t *link;
@@

- handle_to_dev(link->handle)
+ handle_to_dev(link)

@@
@@

- dev_link_t
+ struct pcmcia_device

@@
//typedef client_handle_t;
@@

- client_handle_t
+ struct pcmcia_device *

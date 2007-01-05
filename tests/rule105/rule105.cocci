@@
identifier f, p_dev, link;
@@

f(...,struct pcmcia_device *p_dev,...) {
  ...
- dev_link_t *link = dev_to_instance(p_dev);
  <...
- link
+ p_dev
  ...>
}

@@
@@

- dev_link_t
+ struct pcmcia_device

@@
identifier link, f, handle;
expression E1, E2;
@@

f(...,dev_link_t *link,...) {
  ...
- client_handle_t handle = link->handle;
  <...
(
- first_tuple(handle,E1,E2)
+ first_tuple(link,E1,E2)
|
- pcmcia_request_io(handle,E1)
+ pcmcia_request_io(link,E1)
|
- pcmcia_request_irq(handle,E1)
+ pcmcia_request_irq(link,E1)
|
- pcmcia_request_configuration(handle,E1)
+ pcmcia_request_configuration(link,E1)
|
- pcmcia_request_window(handle,E1,E2)
+ pcmcia_request_window(link,E1,E2)
|
- pcmcia_disable_device(handle)
+ pcmcia_disable_device(link)
|
- pcmcia_get_first_tuple(handle,E1)
+ pcmcia_get_first_tuple(link,E1)
|
- pcmcia_get_next_tuple(handle,E1)
+ pcmcia_get_next_tuple(link,E1)
|
- pcmcia_get_tuple_data(handle,E1)
+ pcmcia_get_tuple_data(link,E1)
|
- pcmcia_parse_tuple(handle,E1,E2)
+ pcmcia_parse_tuple(link,E1,E2)
|
- cs_error(handle,E1,E2)
+ cs_error(link,E1,E2)
|
- handle_to_dev(handle)
+ handle_to_dev(link)
)
  ...>
}

@@
dev_link_t *link;
expression E1, E2;
@@

(
- first_tuple(link->handle,E1,E2)
+ first_tuple(link,E1,E2)
|
- pcmcia_request_io(link->handle,E1)
+ pcmcia_request_io(link,E1)
|
- pcmcia_request_irq(link->handle,E1)
+ pcmcia_request_irq(link,E1)
|
- pcmcia_request_configuration(link->handle,E1)
+ pcmcia_request_configuration(link,E1)
|
- pcmcia_request_window(link->handle,E1,E2)
+ pcmcia_request_window(link,E1,E2)
|
- pcmcia_disable_device(link->handle)
+ pcmcia_disable_device(link)
|
- pcmcia_get_first_tuple(link->handle,E1)
+ pcmcia_get_first_tuple(link,E1)
|
- pcmcia_get_next_tuple(link->handle,E1)
+ pcmcia_get_next_tuple(link,E1)
|
- pcmcia_get_tuple_data(link->handle,E1)
+ pcmcia_get_tuple_data(link,E1)
|
- pcmcia_parse_tuple(link->handle,E1,E2)
+ pcmcia_parse_tuple(link,E1,E2)
|
- cs_error(link->handle,E1,E2)
+ cs_error(link,E1,E2)
|
- handle_to_dev(link->handle)
+ handle_to_dev(link)
)

@@
//typedef client_handle_t;
@@

- client_handle_t
+ struct pcmcia_device *
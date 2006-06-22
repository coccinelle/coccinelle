@@
?local function read_fn;
?local function write_fn;
?local function write_block_fn;
!type T;

identifier dev;
identifier subaddr;
@@

-     read_fn(T *dev, unsigned char subaddr) {
-       ...
- not   while (...) {
-         ooo
- 	  <ooo
- 	  i2c_sendbyte(...)
-           ooo>
-         ooo
-           <ooo
- 	  i2c_readbyte(...)
- 	  ooo>
-         ooo
- not   }
-       ...
-     }

@@
identifier dev;
identifier subaddr;
identifier data;
@@

-     write_fn(T *dev, unsigned char subaddr, unsigned char data) {
-       ...
- not   while (...) {
-         <ooo                         WHEN != i2c_readbyte(...)
-         i2c_sendbyte(...)
-         ooo>
- not   }
-       ...
-     }

@@
identifier dev;
identifier data;
identifier len;
@@

- write_block_fn(T *dev, unsigned char *data, unsigned int len) {
-   <ooo                       WHEN != i2c_readbyte(...)
-   while(...) {
-     <ooo                     WHEN != i2c_readbyte(...)
-     i2c_sendbyte(...)
-     ooo>
-   }
-   ooo>
- }
@@
!local function attach_fn, detach_fn, command_fn;
expression E, E1, E2, E3, E4;
struct i2c_driver I;
@@

ooo        WHEN !<= I.{attach,detach,command} = E;
struct i2c_driver i2c_driver_struct = {
  E1, E2, E3, E4, attach_fn, detach_fn, command_fn
};
ooo

@@
!local function init_fn, exit_fn;
@@

module_init(init_fn);
ooo
module_exit(exit_fn);

@@
!identifier i2c_driver_struct;
@@

init_fn(...) {
  ...
  i2c_register_driver(&i2c_driver_struct);
  ...
}
@@
filename A;
fresh identifier A##_probe;
fresh identifier normal_i2c, normal_i2c_range, probe, probe_range, ignore,
ignore_range, force, addr_data, client_template;
@@

--- a/.../A##.c
+++ b/.../A##.c

-! #include <linux/i2c-old.h>
+  #include <linux/i2c.h>
+  ...
+  static unsigned short normal_i2c[] = {34>>1, I2C_CLIENT_END };
+  static unsigned short normal_i2c_range[] = { I2C_CLIENT_END };
+  static unsigned short probe[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
+  static unsigned short probe_range[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
+  static unsigned short ignore[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
+  static unsigned short ignore_range[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
+  static unsigned short force[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
+  
+  static struct i2c_client_address_data addr_data = {
+          normal_i2c, normal_i2c_range,
+          probe, probe_range,
+          ignore, ignore_range,
+          force
+  };
+  static struct i2c_client client_template;


@@
identifier bus;
@@
   struct T {
     ...
!-   struct i2c_bus *bus;
+    struct i2c_client *client;
     ...
+    struct semaphore lock;
   }

@@
expression E;
@@

!  static struct i2c_driver i2c_driver_struct = {
     E,
-    I2C_DRIVERID_VIDEODECODER,
-    I2C_##A/u, I2C_##A/u + 1,
+    I2C_DRIVERID_##A/u,
+    I2C_DF_NOTIFY,
-    attach_fn,
+    probe_fn,
     detach_fn,
     command_fn
   };
+  static struct i2c_client client_template = {
+    "##A##_client", -1, 0, 0, NULL, &i2c_driver_struct
+  };
@@
@@

  init_fn(...) {
    ...
-   C[i2c_register_driver(&i2c_driver_struct)]
+   C[i2c_add_driver(&i2c_driver_struct)]
    ...
  }

@@
@@

  exit_fn(...) {
    ...
-   C[i2c_unregister_driver(&i2c_driver_struct)]
+   C[i2c_del_driver(&i2c_driver_struct)]
    ...
  }

error words = [i2c_register_driver, i2c_unregister_driver]
@@
identifier device;
identifier coder;
fresh identifier adap, addr, flags, kind;
fresh identifier client;
expression E;
error expression error_code;
@@

-  attach_fn(struct i2c_device *device)
+  attach_fn(struct i2c_adapter *adap, int addr, unsigned long flags, int kind)
 {
+   struct i2c_client *client;
  
+   client = kmalloc(sizeof(*client), GFP_KERNEL);
+   if(client == NULL) return -ENOMEM;
+   client_template.adapter = adap;
+   client_template.addr = addr;
+   memcpy(client, &client_template, sizeof(*client));
    ...
      <...
+     kfree(client);
      return error_code;
      ...>
    ...
+   init_MUTEX(&coder->lock);
+   i2c_attach_client(client);
+   MOD_INC_USE_COUNT;
    return E;
 }
 {
    ooo
      <...
-     MOD_INC_USE_COUNT
      ...>
    ooo
      <...
-     MOD_DEC_USE_COUNT
      ...>
    ooo
 }
 {
    ...
!   T *coder;
    ...
-   device->data = coder;
    ...
      <...
-     device->data
+     coder
      ...>
 }
 {
    ...
+   client->data = coder;
    coder->bus = device->bus;
    ...
 }
 {
   ooo
     <...
-    device->bus
+    client
     ...>
   ooo
     <...
-    coder->bus
+    coder->client
     ...>
   ooo
     <...
-    device->addr
+    addr
     ...>
   ooo
 }
 {
   <...
-  device
+  client
   ...>
 }

error words = [attach_fn: device->data]
@@
identifier device;
fresh identifier client;
@@

(
  attach_fn(struct i2c_device *device) {
|
  command_fn(struct i2c_device *device, ...) {
)
    <...
    device_fn(device,...)
    ...>
  }

@@
@@
  
- device_fn(struct i2c_device *device, ...) {
+ device_fn(struct i2c_client *client, ...) {
    <...
-   device
+   client
    ...>
  }
@@
fresh identifier adap;
@@

+ static int A##_probe(struct i2c_adapter *adap) {
+   return i2c_probe(adap, &addr_data, A##_attach);
+ }
  ...
@@
fresh identifier client;
@@

- detach_fn(struct i2c_device *device)
+ detach_fn(struct i2c_client *client)
  {
    <...
-   device
+   client
    ...>
  }
  {
+   i2c_detach_client(client);
    ...
+   kfree(client);
    MOD_DEC_USE_COUNT;
    ...
  }
@@
identifier device;
fresh identifier client;
@@

- command_fn(struct i2c_device *device, ...) {
+ command_fn(struct i2c_client *client, ...) {
    <...
-   device
+   client
    ...>
  }
@@
local function fn;
identifier client;
expression X1, Y1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, X5, Y5, Z5, X6, Y6, Z6;
@@

(
  fn(...) {
    struct i2c_client *client;
|
  fn(..., struct i2c_client *client, ...) {
)
    ooo
      <...
-     read_fn(X1,Y1)
+     i2c_smbus_read_byte(client)
      ...>
    ooo
      <...
-     write_fn(X2,Y2,Z2)
+     i2c_smbus_write_byte(client,Y2,Z2)
      ...>
    ooo
      <...
-     write_block_fn(X3,Y3,Z3)
+     i2c_master_send(client,Y3,Z3)
      ...>
    ooo
  }

  fn(...) {
    ooo
      <...
-     read_fn(X4,Y4)
+     i2c_smbus_read_byte(X4->client)
      ...>
    ooo
      <...
-     write_fn(X5,Y5,Z5)
+     i2c_smbus_write_byte(X5->client,Y5,Z5)
      ...>
    ooo
      <...
-     write_block_fn(X6,Y6,Z6)
+     i2c_master_send(X6->client,Y6,Z6)
      ...>
    ooo
  }

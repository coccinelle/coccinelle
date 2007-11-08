// shows that there is a problem with \+

@@
identifier fn1, fld1;
identifier data, device;
type T;
expression delay;
@@

  fn1 (
-     void *data
+     struct work_struct *work
     ) {
      <+...
      schedule_delayed_work(&device->fld1,delay);
      ...+>
  }

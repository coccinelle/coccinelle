collect <local function write_fn> = {
  struct usb_serial_device_type s;
  match [[ s.write = write_fn ]]
}

parameter A;
identifier B;
statement S1, S2;

- write_fn(A, int B, ...) {
+ write_fn(A, ...) {
    ...
      ...
(
-     if (B) S1 else S2
+     S2
|
-     B
+     0
)
      ...
}

error words = [write_fn(...)]

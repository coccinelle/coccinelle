@@
struct BCState *bcs;
identifier hscx;
@@

- u8 hscx = bcs->hw.hscx.hscx;
  ... when != hscx
- hcsx
+ bcs->unit
  ... when != hscx

@@
struct BCState *bcs;
@@

(
- bcs->hw.hscx.hscx
+ bcs->unit
|
- bcs->hw.hdlc.rcvidx
+ bcs->rcvidx
|
- bcs->hw.hdlc.rcvbuf
+ bcs->rcvbuf
|
- bcs->hw.w6692.rcvidx
+ bcs->rcvidx
|
- bcs->hw.w6692.rcvbuf
+ bcs->rcvbuf
)

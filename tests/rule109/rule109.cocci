@@
struct BCState *bcs;
identifier hscx_id;
identifier f;
@@

f(...) {
  ...
- u8 hscx_id = bcs->hw.hscx.hscx;
  ... when != hscx_id
- hscx_id
+ bcs->unit
  ... when != hscx_id //should extend to the fn end without putting the fn decl
}

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

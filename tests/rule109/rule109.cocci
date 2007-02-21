@@
struct BCState *bcs_exp;
identifier hscx_id;
identifier f;
@@

f(...) {
  ...
- u8 hscx_id = bcs_exp->hw.hscx.hscx;
  ... when != hscx_id
- hscx_id
+ bcs_exp->unit
  ... when != hscx_id //should extend to the fn end without putting the fn decl
}

@@
struct BCState *bcs_exp;
@@

(
- bcs_exp->hw.hscx.hscx
+ bcs_exp->unit
|
- bcs_exp->hw.hdlc.rcvidx
+ bcs_exp->rcvidx
|
- bcs_exp->hw.hdlc.rcvbuf
+ bcs_exp->rcvbuf
|
- bcs_exp->hw.w6692.rcvidx
+ bcs_exp->rcvidx
|
- bcs_exp->hw.w6692.rcvbuf
+ bcs_exp->rcvbuf
)

@@
struct IsdnCardState *cs; // help the type checker along...
@@

(
 cs->bcs->                // bcs needs to be pure...
-         hw.hscx.hscx
+         unit
|
  cs->bcs->
-          hw.hdlc.rcvidx
+          rcvidx
|
  cs->bcs->
-          hw.hdlc.rcvbuf
+          rcvbuf
|
  cs->bcs->
-          hw.w6692.rcvidx
+          rcvidx
|
  cs->bcs->
-          hw.w6692.rcvbuf
+          rcvbuf
)

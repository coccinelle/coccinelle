@@
//identifier rule3.d_fill_fifo;
struct IsdnCardState *cs;
identifier E;
@@

 	... when != cs->DC_Send_Data
(
+	cs->DC_Send_Data = d_fill_fifo;
	cs->DC_Close = E;
|
+	cs->DC_Send_Data = &d_fill_fifo;
	cs->DC_Close = &E;
)
 	... when != cs->DC_Send_Data

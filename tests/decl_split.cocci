@@
symbol i, x, y;
@@

// TODO - this test fails because coccinelle cannot remove two symbols at once
// in this way

int func(int i) { 
-        int x, y;
//-    int x;
}

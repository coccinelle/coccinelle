@@
symbol i, x, y;
@@

// TODO - this test is known to fail because coccinelle cannot remove two
// symbols at once in this way

int func(int i) { 
-        int x, y;
//-    int x;
}

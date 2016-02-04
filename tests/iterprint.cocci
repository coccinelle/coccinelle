@@
expression bitmap, size;
identifier bit;
statement S1;
iterator name for_each_set_bit;
@@
-for(bit = 0; bit < size; bit++)
+for_each_set_bit(bit, bitmap, size)
-{
-   if (test_bit(bit, bitmap))
        S1
-}

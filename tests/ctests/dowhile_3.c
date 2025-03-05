int fun() {
    int i = 1;
    int* ptr;
    ptr = kmalloc(100);
    do {
        *(ptr + i) = i++;
    } while (i < 100);
   
   return 0;
}

int fun() {
    int i = 1, j = 1;
    do {
       i++; 
        for (j = 1; j < 100; j++) {
            ++j;
        }
    } while (i < 100);
    return 0;
}

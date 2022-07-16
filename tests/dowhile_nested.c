int fun() {
    int i = 1, j = 1;
    do {
       i++; 
        do {
            j++;
        } while (j < 100);
        printf(“end of innner dowhile”);
    } while (i < 100);
    return 0;
}

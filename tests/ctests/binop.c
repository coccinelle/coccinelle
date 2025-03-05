void main() {
    int i = 0;
    int j = 0;

    if( (i = j) + 0 ) {
        i = j;
    }
    if( (i = j) + 0 != 0 ) {
        i = j;
    }
}

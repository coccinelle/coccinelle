int g(int u, int v) {
    int k = u / v;
    return k;
}

int f(int xx, int yy) {
    int i = xx * yy;
    i = g(xx, yy) + i;
    return i;
}

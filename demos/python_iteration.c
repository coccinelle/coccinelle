static void *f(void) {
    return ERR_PTR(foo);
}

static void *g(void) {
    return f();
}

void h(void) {
    void *x;
    x = g();
    x->foo = 42;
}

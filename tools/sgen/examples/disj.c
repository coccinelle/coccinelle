int main(int *arg) {
    int m = function(arg);

    // should not match, lacks assignment to 30
    if (*arg < 0) {
        some_function(10);
        m = *arg;
    }

    // should match!
    if (*arg < 1) {
        some_function(20);
        m = 30;
    }

    // should not match, contains some_function call to 0
    if (*arg < 10) {
        m = 30;
        some_function(0);
    }

    // should match!
    if (*arg < 100) {
        this(5);
    }

    return m;
}


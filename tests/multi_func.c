void fn1(int i) {
 foo_lock();
 i++;
}


void fn2(int i) {
 foo_unlock();
 i++;
}


void fn3(int i) {
 i++;
}

int main() {
  mutex_lock(a);
  {}
  mutex_unlock(a);
  mutex_lock(a);
  y();
  mutex_unlock(a);
}

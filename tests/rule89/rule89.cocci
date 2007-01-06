@@ expression X; @@

  rcu_read_lock()
  <...
(
\+- rcu_dereference(__in_dev_get(X))
+ __in_dev_get_rcu(X)
|
\+- __in_dev_get(X)
+ __in_dev_get_rcu(X)
)
  ...>
  rcu_read_unlock()

@@ expression X, Y; @@

  read_lock(Y);
+ rcu_read_lock();
  <...
(
\+- rcu_dereference(__in_dev_get(X))
+ __in_dev_get_rcu(X)
|
\+- __in_dev_get(X)
+ __in_dev_get_rcu(X)
)
  ...>
+ rcu_read_unlock();
  read_unlock(Y);

@@
expression X;
identifier f;
@@

f(...) {
+ rcu_read_lock();
  <... when != rcu_read_lock();
(
\+- rcu_dereference(__in_dev_get(X))
+ __in_dev_get_rcu(X)
|
\+- __in_dev_get(X)
+ __in_dev_get_rcu(X)
)
  ...>
+ rcu_read_unlock();
}

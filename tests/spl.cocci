// this illustrates how when strict is not so effective.  if the comment is
// removed, only the first return gets its spin_unlock, because the pattern
// is not found under the then of the first if in the C code.  it is not
// good enough that there is a matching pattern around the then

@@
expression l;
@@

spin_lock(l);
... when any
//    when strict
if (...) {
+   spin_unlock(l);
    return ...;
}

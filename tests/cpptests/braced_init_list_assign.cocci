# spatch --c++
// remove an assignment of a braced-init-list (initializer-list within brackets)
@@
@@
- i = {0};

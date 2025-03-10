# spatch --c++
// using namespace ns-name;
@@
identifier i;
@@
- using namespace i;

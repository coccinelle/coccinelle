#spatch --c++
// In C++, and, or, not, xor, compl, bitand, bitor, and_eq, or_eq, xor_eq,
// and not_eq are operators, so the function using them must parse.

@@
@@

- g();
+ h();

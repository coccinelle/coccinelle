// we cant say snd_magic_cast(E1, E2, E3) because E3 
// is not an expression. it is an action, which is not even exactly 
// a statement.
@@
expression E1, E2;
@@
- snd_magic_cast(E1, E2, ...)
+ 4

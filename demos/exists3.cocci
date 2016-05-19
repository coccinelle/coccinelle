// this rule uses exists, with it, the rule is successful if at least
// one of the possible control flows possible match.
// exists3.c has two possible control flows on main():
// 1. b() --> a > 5 --> c();
// 2. b() --> a <= 5 ---> no c();
// The match on 1. enables the changes to take place.
@r exists @
@@

b();
...
-c();

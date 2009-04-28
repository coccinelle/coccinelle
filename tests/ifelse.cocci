@ if_else_3 @
position p_3_1, p_3_2;
expression E_3_1, E_3_2, E_3_3, E_3_4; 
statement S_3_1, S_3_2, S_3_3, S_3_4;
@@
if (E_3_1)
   S_3_1
else if (E_3_2)
   S_3_2
else if@p_3_1 (E_3_3)
   S_3_3
else if@p_3_2 (E_3_4)
   S_3_4
// + foo_3();

@

script:python @ expr_1 << if_else_3.E_3_1;
                expr_2 << if_else_3.E_3_2;
                expr_3 << if_else_3.E_3_3;
                expr_4 << if_else_3.E_3_4;
              @@
print "--- 4"
print expr_1
print expr_2
print expr_3
print expr_4

@ if_else_2 @
position p_2 != if_else_3.p_3_2;
expression E_2_1, E_2_2, E_2_3; 
statement S_2_1, S_2_2, S_2_3;
@@
if (E_2_1)
   S_2_1
else if (E_2_2)
   S_2_2
else if@p_2 (E_2_3)
   S_2_3
// + foo_2();

@

script:python @ expr_1 << if_else_2.E_2_1;
                expr_2 << if_else_2.E_2_2;
                expr_3 << if_else_2.E_2_3;
              @@
print "--- 3"
print expr_1
print expr_2
print expr_3

@ if_else_1 @ 
// @@
// position p1;
expression E_1_1, E_1_2; 
statement S_1_1, S_1_2;
position p_1_1 != if_else_3.p_3_1;
position p_1_2 != if_else_2.p_2;
@@
if@p_1_1 (E_1_1)
   S_1_1
else if@p_1_2 (E_1_2)
   S_1_2
// + foo_1();

@

script:python @ expr_1 << if_else_1.E_1_1;
                expr_2 << if_else_1.E_1_2;
              @@
print "--- 2"
print expr_1
print expr_2

@ if_if_1 @ 
expression E_10_1, E_10_2; 
statement S_10_1, S_10_2;
@@
if (E_10_1)
   S_10_1
if (E_10_2)
   S_10_2
// + bar();
@

script:python @ expr_1 << if_if_1.E_10_1;
                expr_2 << if_if_1.E_10_2;
              @@
print "- 2"
print expr_1
print expr_2


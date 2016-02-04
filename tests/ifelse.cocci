// ifelse.cocci,  3 Aug 09

@ if_else_1
disable neg_if
@
expression E_1_ie;
statement S_1_ie, S_2_ie;
position p_1_ie;
@@
if@p_1_ie (E_1_ie)
   S_1_ie
else
   S_2_ie

@
script:python @ expr_1_ie << if_else_1.E_1_ie;
                loc_1_ie << if_else_1.p_1_ie;
              @@
print "--- ifelse"
print loc_1_ie[0].line, " ", loc_1_ie[0].column, " ", expr_1_ie


@ if_else_if_else
disable neg_if
@
expression E_1, E_2; 
statement S_1, S_2, S_3;
position p_1, p_2, p_3;
@@
if@p_1 (E_1)
   S_1
else if@p_2 (E_2)
   S_2
else
    S_3@p_3

@
script:python @ expr_1 << if_else_if_else.E_1;
                expr_2 << if_else_if_else.E_2;
                loc_1 << if_else_if_else.p_1;
                loc_2 << if_else_if_else.p_2;
                loc_3 << if_else_if_else.p_3;
              @@
print "--- ifelseifelse"
print loc_1[0].line, " ", loc_1[0].column, " ", expr_1
print loc_2[0].line, " ", loc_2[0].column, " ", expr_2
cocci.include_match(False)

@
script:python @ expr_1 << if_else_if_else.E_1;
                expr_2 << if_else_if_else.E_2;
                loc_1 << if_else_if_else.p_1;
                loc_2 << if_else_if_else.p_2;
              @@
print "--- ifelseif"
print loc_1[0].line, " ", loc_1[0].column, " ", expr_1
print loc_2[0].line, " ", loc_2[0].column, " ", expr_2


#spatch
# spatch -D cocci_args_funname=f
#spatch -D
# spatch
#                                       spatch cocci_args_arg=a

@@
identifier virtual.cocci_args_funname;
identifier virtual.cocci_args_arg;
@@

- cocci_args_funname()
+ g(cocci_args_arg)

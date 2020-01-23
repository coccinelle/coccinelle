#spatch
# spatch --opt-c 'inexistant file'
#spatch -D
# spatch
#                                       spatch cocci_args_arg=a
# spatch --ignore-unknown-options --fake-option

@@
identifier virtual.cocci_args_arg;
@@

- f()
+ g(cocci_args_arg)

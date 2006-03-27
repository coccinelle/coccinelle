@@
constant char *string;
@@

- devfs_mk_dir(NULL,string,NULL)
+ devfs_mk_dir(string)

@@
constant char *string;
identifier txt;
int E;
expression list A;
@@

- char txt[E];
  ...                          when != txt
- sprintf(txt,string,A);
  ...                          when != txt
- devfs_mk_dir(NULL,txt,NULL)
+ devfs_mk_dir(string,A)

error words =  [devfs_mk_dir]

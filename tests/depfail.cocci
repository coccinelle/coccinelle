virtual df_first

@df_second depends on df_first@
@@

- foo();

@depends on !df_first@
@@

* bar();

@depends on df_second@
@@

- bar();

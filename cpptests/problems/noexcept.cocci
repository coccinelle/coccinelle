# spatch --c++
@identifier@
symbol foo;
@@

- foo
+ bar

@identifier@
symbol loc;
@@

- loc
+ xyz

@identifier@
symbol args;
@@

- args
+ abc

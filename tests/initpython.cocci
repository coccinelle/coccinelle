@@
expression path;
fresh identifier newpath = script:python (path) { "XXX" + path };
@@
- f(path)
+ f(newpath)
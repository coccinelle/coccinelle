@@
identifier v,fld;
@@

- struct video_tuner v;
+ struct video_tuner *v;
<...
(
-     v.fld
+     v->fld
|
-     v
+     *v
)
...>
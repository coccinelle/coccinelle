/* This file has interleaving of patch and non-patch rules. */

@match1@
declarer name module1;
declarer name module2;
identifier d;
@@

(
  module1(d);
|
  module2(d, ...);
)



@fix1 depends on match1@
identifier match1.d;
@@

  static struct struct_name d = {
-   .field = MACRO,
  };


@match2@
identifier d;
@@

(
  module3(&d)
|
  module4(&d, ...)
|
  module5(&d, ...)
)


@fix2 depends on match2@
identifier match2.d;
@@

  static struct struct_name d = {
-   .name = MACRO,
  };

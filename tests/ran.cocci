@@
field f : script:python () { f == "randomized_struct_fields_end" };
@@

struct task_struct {
  ...
+ int something;
  f
  ...
};

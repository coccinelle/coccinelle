#define APPLY_FOR_ROLE return;
#define APPLY_FOR_ROLE_HINT MACROSTATEMENT

void do_nothing(fn_type<void(void)> fn) {
  APPLY_FOR_ROLE;
  APPLY_FOR_ROLE_HINT;
}

@ parse_mod @
identifier name, args, params, num, level_min, level_max;
identifier unknown, param, val, doing;
type s16;
@@
 char *parse_args(const char *name,
                         char *args,
                         const struct kernel_param *params,
                         unsigned num,
                         s16 level_min,
                         s16 level_max,
+                        void *arg,
                         int (*unknown)(char *param, char *val,
                                        const char *doing
+                                       , void *arg
                                        ))
{
        ...
}

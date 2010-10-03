typedef enum
{
} another_test;

typedef enum
{
xxx
#ifdef FOO
,bar
#endif
} another_test2;

typedef enum
{
xxx
} this_one_works;

static reg_errcode_t
regex_compile ()
{
  unsigned char *pending_exact = NULL;
}

#define __cast_attr MACROANNOTATION

int func()
{
	char *a = (char *__cast_attr) x;
	char *a = (__cast_attr char *) x;
	char *a = (int __cast_attr *) x;
	char *a = (char *__cast_attr) x;
	char *a = (int) x;
}

#define __cast_attr MACROANNOTATION

int func()
{
	char *a = (int __cast_attr) x;
	char *a = (__cast_attr int) x;
	char *a = (int __cast_attr *) x;
	char *a = (int *__cast_attr) x;
	char *a = (int) x;
}

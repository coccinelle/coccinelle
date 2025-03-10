#define __cast_attr MACROANNOTATION

int func()
{
	int *a = (int __cast_attr) x;
	int *a = (__cast_attr int) x;
	int *a = (int __cast_attr *) x;
	int *a = (int *__cast_attr) x;
	int *a = (int) x;
}

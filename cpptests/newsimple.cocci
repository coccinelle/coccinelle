#spatch --c++

@@
expression E;
@@

int main()
{
...
	sizeof(E);
-	new int(1);
+	new int(2);
...
}
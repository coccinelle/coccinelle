# spatch --c++
@@
identifier a,b;
@@
namespace
- a
+ b
{
	namespace
-	b
+	a
	{ };
};

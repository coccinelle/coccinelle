# spatch --c++
// FIXME: broken
// 'public' specifier required to be there in code in order to match
@@ @@
  struct A {
	public:
// note: inserting only a comment would go unnoticed by coccinelle
+       int a;// after public
  };

# spatch --c++
// FIXME: broken
// 'public' specifier required to be there in code in order to match
@@ @@
  struct A {
	public:
+       // after public
  };

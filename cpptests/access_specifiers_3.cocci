# spatch --c++
// FIXME: broken
// add next to 'public' access specifier
@@ @@
  struct A {
	public:
+	private:
  };

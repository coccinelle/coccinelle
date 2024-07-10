# spatch --c++
// remove 'public' access specifier and add 'private'
@@ @@
  struct A {
-  	public:
+  	private:
  };


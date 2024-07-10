# spatch --c++
// remove existing 'public' access specifier
@@ @@
  struct A {
- 	public:
  };

# spatch --c++
// remove existing 'public' access specifier
@@ @@
  class A {
- 	public:
  };

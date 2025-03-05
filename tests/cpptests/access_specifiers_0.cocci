# spatch --c++
// can't remove existing non-existing 'private' access specifier
@@ @@
  struct A {
- 	private:
  };

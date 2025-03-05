#spatch --c++
@@
field lfld;
field list [n={2}] f2fld;
@@
struct str_t { 
- f2fld
  ...
- lfld
};
+ struct l_t { f2fld lfld };

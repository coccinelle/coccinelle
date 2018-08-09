@a@
identifier id1 = {particle_data_extra};
identifier I;
typedef MyDouble;
typedef MyFloat;
type T = {MyDouble,MyFloat}; // whitelist approach
@@
struct id1{ ...
- T I;
+ T*I;
  ...
};

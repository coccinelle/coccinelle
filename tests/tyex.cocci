@@
@@

typedef struct {
   ...
-  char *name;
   ...
}
- Location
+ Point
  ;
+ typedef struct {
+   char *name;
+   Point p;
+ } Location;

@@
typedef Location;
Location some_location;
identifier x;
@@

(
  some_location.name
|
- some_location.x
+ some_location.p.x
)

@@
Location *some_location;
identifier x;
@@

(
  some_location->name
|
- some_location->x
+ some_location->p.x
)

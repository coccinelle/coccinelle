// splitting up the rules rather than just listing a set of possible types
// helps a little bit with the expression ambiguity problem

@@
struct input_dev *E;
@@

(
- E->idbus
+ E->id.bustype
|
- E->idvendor
+ E->id.vendor
|
- E->idproduct
+ E->id.product
|
- E->idversion
+ E->id.version
)

@@
struct input_device_id *E;
@@

(
- E->idbus
+ E->id.bustype
|
- E->idvendor
+ E->id.vendor
|
- E->idproduct
+ E->id.product
|
- E->idversion
+ E->id.version
)

@@
struct gameport *E;
@@

(
- E->idbus
+ E->id.bustype
|
- E->idvendor
+ E->id.vendor
|
- E->idproduct
+ E->id.product
|
- E->idversion
+ E->id.version
)

@@
struct input_dev E;
@@

(
- E.idbus
+ E.id.bustype
|
- E.idvendor
+ E.id.vendor
|
- E.idproduct
+ E.id.product
|
- E.idversion
+ E.id.version
)


@@
struct input_device_id E;
@@

(
- E.idbus
+ E.id.bustype
|
- E.idvendor
+ E.id.vendor
|
- E.idproduct
+ E.id.product
|
- E.idversion
+ E.id.version
)


@@
struct gameport E;
@@

(
- E.idbus
+ E.id.bustype
|
- E.idvendor
+ E.id.vendor
|
- E.idproduct
+ E.id.product
|
- E.idversion
+ E.id.version
)

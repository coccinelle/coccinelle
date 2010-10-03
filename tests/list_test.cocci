@@
@@

f(
- a
 )

@@
@@

g(
- a,
  b
 )

@@
@@

h(
  ...,
- a,
  ...,
  b,
  ...
 )

@@
@@

int f(
- int a
 ) { ... }

@@
@@

int g(
- int a,
  int b
 ) { ... }

@@
@@

int h(
  ...,
- int a,
  ...,
  int b,
  ...
 ) { ... }

@@
@@

int f[] = {
- a
 };

@@
@@

int g[] = {
- a,
  b
 };

@@
@@

int h[] = {
  ...,
- a,
  ...,
  b,
  ...,
 };

@@
@@

int i[] = {
  ...,
  a,
  ...,
- b
 };

@@
@@

struct f {
- int a;
+ int b;
 };

@@
@@

struct g {
- int a;
  int b;
 };

@@
@@

struct h {
  ...
- int a;
  ...
  int b;
  ...
 };

@@
@@

enum f {
- a
+ b
 };

@@
@@

enum g {
- a,
  b
 };

@@
@@

enum h {
  ...,
- a,
  ...,
  b,
  ...
 };

@@
@@

enum i {
  ...,
  a,
  ...,
- b
 };

@@
@@

-#define f(a) 3

@@
@@

- #define g(a,b) 3

@@
@@

- #define h(...,a,...,b,...) 3

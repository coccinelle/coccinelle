@@
@@
- int \(disj_id1\|disj_id2\);
+ int disj_id3;

@@
@@
- disj_expr(\(1\|2\))
+ disj_expr(3)

@@
@@
- \(int\|char\) disj_type;
+ bool disj_type;

@@
@@
- void \(disj_id1\|disj_id2\)();
+ void disj_id3();

@@
@@
- void \(disj_id1\|disj_id2\)() {
+ void disj_id3() {
    ...
  }

@@
@@
(
  void f() {
    ...
- return;
+ break;
    ...
  }
|
  void g() {
    ...
- return;
+ continue;
    ...
  }
)

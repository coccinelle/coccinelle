@@
@@
- int \(disj_id1\|disj_id2\);
+ int disj_id3;

@@
@@
- int \(disj_id1\|disj_id2\) = 0;
+ int disj_id3 = 0;

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
declarer name DISJ_DECLARER1;
declarer name DISJ_DECLARER2;
declarer name DISJ_DECLARER3;
@@
- \(DISJ_DECLARER1\|DISJ_DECLARER2\)(...);
+ DISJ_DECLARER3(0);

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

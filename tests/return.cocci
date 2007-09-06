// not needed with 'return implicit' feature
// @@
// statement S;
// @@
// 
// foo(...) {
//   ...
// (
// + before_return();
//   return;
// |
//   S
// + before_return();
// )
// }


@@
statement S;
@@

foo(...) {
  ...
+ before_return();
  return;
}

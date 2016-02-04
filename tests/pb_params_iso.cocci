// soluce ? 
//  - add isomorphism:
//      func(..., P, ...) => func(P) , => func(P,...), => func(...,P) ?
//      but propagate well the modifiers ?
//      but then how specify that we really want match function
//      that have parameters before ? use a P metavariable.
//  - allow func (... int i ...) in parsing_cocci/

@@
identifier func;
@@

// pad: this one does not work because the last comma is tagged
// and so I force it to be present.
//  func(..., 
// -      int i,
// +      int i, char j,
//       ...) {
//   ...
//  }

 func(..., 
-      int i
+      int i, char j
      ,...) {
  ...
 }


                


// commit 5cbded585d129d0226cb48ac4202b253c781be26
// Author: Robert P. J. Day <rpjday@mindspring.com>
// Date:   Wed Dec 13 00:35:56 2006 -0800
// 
//     [PATCH] getting rid of all casts of k[cmz]alloc() calls
// 
//     Run this:
// 
//         #! /bin/sh
//         for f in $(grep -Erl "\([^\)]*\) *k[cmz]alloc" *) ; do
//           echo "De-casting $f..."
//           perl -pi -e "s/ ?= ?\([^\)]*\) *(k[cmz]alloc) *\(/ = \1\(/" $f
//         done
// 
//     And then go through and reinstate those cases where code is
// casting pointers    to non-pointers.
// 
//     And then drop a few hunks which conflicted with outstanding work.


@@ 
expression E; type T; 
@@
  E = 
-   (T) 
    kmalloc(...)

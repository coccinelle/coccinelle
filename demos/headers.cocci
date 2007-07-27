// This file illustrates the various items that can be used at the beginning
// of a semantic patch, or at the beginning of a rule

// The following illustrates how to include a file of isomorphisms located
// in the default path, as indicated by config.  Uncommenting the following
// will give and error, because standard.iso is already loaded by default
// and it is not allowed to load two isomorphisms with the same name
// using <standard.iso>
using "headers.iso" // an iso file in the current directory

@ rule0 @
@@

- a
+ b

@
rule1  // rule name
extends rule0 // inherit the metavariables from rule0
depends on rule0 && !rule0 // now this rule will never be applied ...
using "headers2.iso" // more iso files can be included, separated by commas
disable three, drop_cast // isos should apply to f and x, but not m
@
@@

(
- f(3)
+ fff(12)
|
- x(3)
+ xxx(12)
|
- m(3)
+ mmm(12)
)

@
rule2  // rule name
extends rule0 // inherit the metavariables from rule0
depends on rule0 && !rule0 // now this rule will never be applied ...
using "headers2.iso" // more iso files can be included, separated by commas
@
@@

(
- f(3)
+ fff(12)
|
- x(3)
+ xxx(12)
|
- m(3)
+ mmm(12)
)

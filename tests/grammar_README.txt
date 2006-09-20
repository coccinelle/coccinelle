Rule $n
------------------------------------------------------------------------------
Cocci file : $filename.cocci

Bug: $n     // the number of bugs found by spatch by, i.e., where spatch
            // did the right thing and the .res file did not
Bugfix: $n  // the number of known (from other sources) update bugs made by
            // others but done correctly by spatch
------------------------------------------------------------------------------
Note: // such as name of files not in SOURCES but in current dir, and the
      // reason why
------------------------------------------------------------------------------

$filename.c
  * [status]  ok|spatch-ok|fail|wrong|UNKNOWN
  * [note] bugfix=$n
  * [comment] ...
  * [todo_hint] parser|cpp|iso|ctl|pad|julia|rene|pad|pad|pad
  * [exn] ...
  * [bug] bug|bugfix ...



------------------------------------------------------------------------------

// Only the Cocci file : ...  and the [status] lines are mandatory

// Categories:
//
//   ok        : when the SP applies cleanly, i.e., there are no other
//               changes, no bugs, etc. Differences in comments and
//               whitespace _may_ be acceptable; if in doubt use
//               spatch-ok; typically this category is only used for
//               .ok -files
//   spatch-ok : when the SP did as expected but there were other
//               changes and/or bugs; typically this category is used
//               for manually reviewed .failed files
//   fail      : spatch fails for some reason (and throws an exception)
//   wrong     : spatch does not fail, but does not produce the
//               "correct" and/or expected result either; this
//               indicates either an error (or lacking feature) in
//               spatch or possibly bugs made by the maintainer
//   UNKNOWN   : self-explanatory
//

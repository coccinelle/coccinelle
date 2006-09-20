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

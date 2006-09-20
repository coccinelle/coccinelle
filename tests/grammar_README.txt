Cocci file : $filename.cocci

Note: // such as name of files not in SOURCES but in current dir, and the
      // reason why

------------------------------------------------------------------------------

$filename.c
 [status]  ok|spatch-ok|fail|wrong|UNKNOWN
 [note] bugfix=$n
 [comment] ...
 [todo_hint] parser|cpp|iso|ctl|pad|julia|rene|pad|pad|pad
 [exn] ...



------------------------------------------------------------------------------

// Only the Cocci file : ...  and the [status] lines are mandatory

open Common

(* full_engine takes (coccifile, isofile) and cfiles in parameters and
 * returns a list associating to the input cfiles, and maybe header
 * files that was also required to be modified, the files containing the
 * result, in general files in /tmp.
 * 
 * This function use memoisation internally, which is useful when 
 * use -dir to not redo twice the same work. So take care!
 *)
val full_engine : 
  (filename * filename) -> filename list -> 
  (filename * filename option) list

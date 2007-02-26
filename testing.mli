open Common

val testone : 
 string (* test file *) -> bool (* compare_expected *) -> string (* iso *) -> 
 string (* output file *) -> unit

val testall : string (* iso *) -> unit



val print_diff_expected_res_and_exit : filename -> filename -> bool -> unit


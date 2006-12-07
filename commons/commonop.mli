val ( +> ) : 'a -> ('a -> 'b) -> 'b
val ( =~ ) : string -> string -> bool
val ( ==~ ) : string -> Str.regexp -> bool

(* for profiling =~ ugly but necessary because of the split of file *)
val _match_func : (string -> string -> bool) ref

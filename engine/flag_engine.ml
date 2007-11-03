let debug_engine = ref false
let debug_unparsing = ref false

(* false = simpler formulas, only for debugging *)
let useEU = ref true

let disallow_nested_exps = ref false

(* if this flag is not set, then break and continue are also error exits *)
let only_return_is_error_exit = ref false

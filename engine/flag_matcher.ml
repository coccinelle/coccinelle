(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

let debug_engine = ref false
let verbose_matcher = ref true


(* false = simpler formulas, only for debugging *)
let useEU = ref true

let disallow_nested_exps = ref false

(* if this flag is not set, then break and continue are also error exits *)
let only_return_is_error_exit = ref false

(* a hack to allow adding code in some more sgrep-like uses *)
let allow_inconsistent_paths = ref false

(* see the use of this variable in asttoctl2.ml *)
let no_safe_expressions = ref false

let show_misc = ref true


let show_transinfo = ref false

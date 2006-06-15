open Common open Commonop
(* 
 can either:
  - do also a kind of inferer
  - extract the information from the .h files
*)


open Ast_c

type environment
type context = fullType option

let rec (annotate_expr: environment -> context -> expression -> expression) = fun env ctx -> 
  raise Todo


let rec (annotate_program: environment -> program -> program) = fun env ctx -> 

  (* use visitor_synth *)
  raise Todo

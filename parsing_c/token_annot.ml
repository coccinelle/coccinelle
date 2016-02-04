(* Provides a dictionary of possible annotations on tokens, indexed by keys.
 *
 * The purpose of these annotations is to direct the pretty printing of
 * tokens. The annotations can be set by AST transformations.
 *
 * Assumptions: only a few tokens have annotations, and those have only
 * a few of them.
 *)

type annot_key =
    Exclude_start
  | Exclude_end

type annot_val =
  Unit

(* A linked list should offer a good tradeoff between space usage
 * and lookup overhead given our assumptions.
 *)
type annots = (annot_key * annot_val) list

let empty = []

let get_annot anns key =
  if List.mem_assoc key anns
  then Some (List.assoc key anns)
  else None

let put_annot key value anns =
  (key, value) :: anns

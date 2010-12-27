type init_info = (string (* language *) * string (* rule name *)) *
      string list (* defined virtual rules *)

let initialization_stack = ref ([] : init_info list)

(* ----------------------------------------------------------------------- *)

let base_file_list = ref ([] : string list)
let parsed_virtual_rules = ref ([] : string list)
let parsed_virtual_identifiers = ref ([] : string list)

(* ----------------------------------------------------------------------- *)

type pending_info = string list (* files to treat *) *
      string list * (* defined virtual rules *)
      (string * string) list (* virtual identifiers *)

let pending_instances = ref ([] : pending_info list)

let add_pending_instance x =
  pending_instances := !pending_instances @ [x]
					      
let get_pending_instance _ =
  match !pending_instances with
    [] -> None
  | x::xs ->
      pending_instances := xs;
      Some x

(* ----------------------------------------------------------------------- *)

let check_virtual_rule r =
  if not (List.mem r !parsed_virtual_rules)
  then failwith ("unknown virtual rule "^r)

let check_virtual_ident i =
  if not (List.mem i !parsed_virtual_identifiers)
  then failwith ("unknown virtual rule "^i)

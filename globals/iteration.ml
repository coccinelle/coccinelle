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

let pending_instances_file = ref ([] : pending_info list)
let pending_instances_dir = ref ([] : pending_info list)

let add_pending_instance (files,a,b) =
  match files with
    None ->
      pending_instances_dir := (!base_file_list,a,b) :: !pending_instances_dir
  | Some f ->
      (* if one specifies a file, it is assumed to be the current one *)
      (* put at the end of the list of information about this file *)
      let rec loop = function
	  [] -> [(f,a,b)]
	| ((f1,a1,b1) as front)::rest ->
	    if f = f1
	    then front :: (loop rest)
	    else (f,a,b) :: front :: rest in
      pending_instances_file := loop !pending_instances_file
					      
let get_pending_instance _ =
  (if (List.length !pending_instances_file) > 0 or
    (List.length !pending_instances_dir) > 0
  then
    Common.pr2
      (Printf.sprintf
	 "%d pending new file instances\n%d pending original file instances\n"
	 (List.length !pending_instances_file)
	 (List.length !pending_instances_dir)));
  match !pending_instances_file with
    [] ->
      (match !pending_instances_dir with
	[] -> None
      | x::xs ->
	  pending_instances_dir := xs;
	  Some x)
  | x::xs ->
      pending_instances_file := xs;
      Some x

(* ----------------------------------------------------------------------- *)

let check_virtual_rule r =
  if not (List.mem r !parsed_virtual_rules)
  then failwith ("unknown virtual rule "^r)

let check_virtual_ident i =
  if not (List.mem i !parsed_virtual_identifiers)
  then failwith ("unknown virtual rule "^i)

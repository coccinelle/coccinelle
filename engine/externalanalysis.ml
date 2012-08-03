(* Support code for the integration of results
 * from external analysis tools.
 *
 * the file should contain a result per line, where each line
 * is of the form:
 *   filename;begin_row;begin_column;end_row;end_column;data
 * where data can be:
 *   E;n;int_1;...;int_n      (an integer set)
 *   I;bnd;bnd     (integer bounds, either integer or empty)
 *   other
 *)

module PoslMap = Map.Make
  (struct
    type t      = Ast_c.posl
    let compare = Ast_c.compare_posl
  end)

module StringMap = Map.Make (String)

type result =
    IntSet      of int64 list
  | IntBounds   of int64 option * int64 option
  | Other       of string

type result_map = (((result list) PoslMap.t) PoslMap.t) StringMap.t

let empty_map : result_map = StringMap.empty

let current_map = ref empty_map

let loc_regexp       = Str.regexp "\\([^;]*\\);\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\);\\(.+\\)"
let intset_regexp    = Str.regexp "E;\\([0-9]+\\)\\(\\(;[0-9]+\\)*\\)"
let intbounds_regexp = Str.regexp "I;\\([0-9]*\\);\\([0-9]*\\)"
let split_regexp     = Str.regexp "[;]"

(* skips over unparsable entries *)
let load_external_results filename =
  let chan = open_in filename in
  try while true do
    let line = input_line chan in
    match Str.string_match loc_regexp line 0 with
      false -> ()
    | true  ->
	let s_file   = Str.matched_group 1 line in
	let s_lbegin = Str.matched_group 2 line in
	let s_cbegin = Str.matched_group 3 line in
	let s_lend   = Str.matched_group 4 line in
	let s_cend   = Str.matched_group 5 line in
	let s_data   = Str.matched_group 6 line in
	let mk_posl s_row s_col =
          (int_of_string s_row, int_of_string s_col) in
	let posl_begin = mk_posl s_lbegin s_cbegin in
	let posl_end   = mk_posl s_lend s_cend in

	(* map the line to a value *)
	let value =
	      match Str.string_match intset_regexp s_data 0 with
		true  ->
		  let n_fields = int_of_string (Str.matched_group 1 s_data) in
		  let s_fields = Str.matched_group 2 s_data in
		  let ints = Str.bounded_split split_regexp s_fields n_fields in
		  IntSet (List.map Int64.of_string ints)
	      | false ->
		  match Str.string_match intbounds_regexp s_data 0 with
		    true  -> let mk_bound s =
		                   match String.length s == 0 with
				     true  -> None
				   | false -> Some (Int64.of_string s) in
		             IntBounds (mk_bound (Str.matched_group 1 s_data),
					mk_bound (Str.matched_group 2 s_data))
		  | false -> Other s_data in

	(* add the new value to the current map *)
	let ensure_str m k f =
	      let v = match StringMap.mem k m with
		        true  -> f (StringMap.find k m)
	              | false -> f PoslMap.empty in
	      StringMap.add k v m in
	let ensure_posl k e f m =
	      let v = match PoslMap.mem k m with
		        true  -> f (PoslMap.find k m)
	              | false -> f e in
	      PoslMap.add k v m in

	current_map := ensure_str !current_map s_file
	  (ensure_posl posl_begin PoslMap.empty
             (ensure_posl posl_end [] (fun xs -> value :: xs )))
  done with End_of_file -> ();
  close_in chan

let find_key find_smaller m k =
  match PoslMap.split k m with
    (_, Some r, _) -> r
  | (smaller, None, greater) ->
      let (_, r) =
        match find_smaller with
	  true  -> PoslMap.max_binding smaller
	| false -> PoslMap.min_binding greater in
      r

(* finds all nearest results in the map that enclose the given position *)
let find_results filename p_begin p_end =
  let m_begin = StringMap.find filename !current_map in
  let m_end   = find_key true m_begin p_begin in
  let results = find_key false m_end p_end in
  results

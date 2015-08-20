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
 *
 * Note: for the moment the analysis results are assumed to be
 * integer ranges or sets. Other types of analysis results will
 * be regarded as a plain string.
 *
 * Todo: implement a proper querying facility that keeps different
 * types of analysis apart.
 *)


(* provides a search structure for providing a map from posl to
 * some value, and search operations that find the nearest posl.
 * This is mainly a hack for backwards compatibility with older
 * ocaml versions that provide only limited functionality on
 * Maps.
 *)
module PoslMap = struct
  module PComp = struct
    type t      = Ast_c.posl
    let compare = Ast_c.compare_posl
  end

  module PMap = Map.Make (PComp)
  module PSet = Set.Make (PComp)

  type 'a t = (PSet.t * 'a PMap.t)

  let empty          = (PSet.empty, PMap.empty)
  let mem x (s, _)   = PSet.mem x s
  let find k (_, m)  = PMap.find k m
  let add k v (s, m) = (PSet.add k s, PMap.add k v m)

  (* throws Not_found if such a key does not exist *)
  let nearest_key find_smaller k s =
    match PSet.split k s with
      (_, true, _) -> k
    | (smaller, false, greater) ->
      match find_smaller with
	true  -> PSet.max_elt smaller
      | false -> PSet.min_elt greater

  (* throws Not_found if such an entry does not exist *)
  let find_nearest find_smaller (s, m) k =
    PMap.find (nearest_key find_smaller k s) m
end

module StringMap = Map.Make (String)

module Int64Set = Set.Make (Int64)

(* bound: either concrete or unbounded *)
type bound = int64 option

(* The type of analysis results, which for the moment focuses on integers.
 * The lower bound should be smaller or equal to the upper bound (not enforced)
 *)
type result =
    IntSet      of Int64Set.t
  | IntBounds   of bound * bound
  | Other       of string

(* for printing *)
let show_bound b =
  match b with
    None   -> "*"
  | Some i -> Printf.sprintf "%Ld" i

let show_result result =
  let out = Buffer.create 120 in
  begin match result with
    IntSet s ->
      Buffer.add_string out "{";
      Int64Set.iter (fun i ->
	Buffer.add_string out (Printf.sprintf "%Ld;" i)) s;
      Buffer.add_string out "}"
  | IntBounds (l, u) ->
      Buffer.add_string out (show_bound l);
      Buffer.add_string out "-";
      Buffer.add_string out (show_bound u)
  | Other s ->
      Buffer.add_string out s
  end;
  Buffer.contents out

(* search structure for results *)
type result_map = (((result list) PoslMap.t) PoslMap.t) StringMap.t

let empty_map : result_map = StringMap.empty

(* this module is organized that it contains the analysis results as a singleton. *)
let current_map = ref empty_map

(* regular expressions for extracting results from the .csv file *)
let loc_regexp       = Str.regexp "\\([^;]*\\);\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\);\\(.+\\)"
let intset_regexp    = Str.regexp "E;\\([0-9]+\\)\\(\\(;[0-9]+\\)*\\)"
let intbounds_regexp = Str.regexp "I;\\([0-9]*\\);\\([0-9]*\\)"
let split_regexp     = Str.regexp "[;]"

(* Loading of results from a .cvs-like format.
 * Skips over unparsable entries without reporting an error.
 *)
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
		  let strs = Str.bounded_split split_regexp s_fields n_fields in
		  let ints = List.map Int64.of_string strs in
		  let set = List.fold_right Int64Set.add ints Int64Set.empty in
		  IntSet set
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



(* finds all nearest results in the map that enclose the given position *)
let find_results filename p_begin p_end =
  try
    let m_begin = StringMap.find filename !current_map in
    let m_end   = PoslMap.find_nearest true m_begin p_begin in
    let results = PoslMap.find_nearest false m_end p_end in
    results
  with Not_found -> []


(*
 * some convenience functions on analysis results.
 *)

let within_bounds c l u =
  match (l, u) with
    (None, None)     -> true
  | (None, Some k)   -> c <= k
  | (Some k, None)   -> k <= c
  | (Some k, Some n) -> k <= c && c <= n

let contains_bounds m n l u =
  begin
    match (l, m) with
      (None, None)     -> true
    | (Some k, Some i) -> k <= i
    | _                -> false
  end && begin
    match (u, n) with
      (None, None)     -> true
    | (Some q, Some j) -> j <= q
    | _                -> false
  end

(* given two result values, computes their intersection. An empty intersection
   is indicated with a None result value.
*)
let intersect_results r1 r2 =
  let sets s1 s2 =
    match Int64Set.inter s1 s2 with
      s when Int64Set.is_empty s -> None
    | s                          -> Some (IntSet s) in
  let bounds_set r l u s =
    if Int64Set.for_all (fun c -> within_bounds c l u) s
    then Some r
    else None in
  let bounds r m n l u =
    if contains_bounds m n l u
    then Some r
    else None in
  match r1 with
    IntSet s1 -> begin
      match r2 with
	IntSet s2        -> sets s1 s2
      | IntBounds (l, u) -> bounds_set r2 l u s1
      | Other _          -> None
      end
  | IntBounds (l, u) -> begin
      match r2 with
	IntSet s2        -> bounds_set r1 l u s2
      | IntBounds (m, n) -> bounds r1 l u m n
      | Other _          -> None
      end
  | Other _ -> None

(* a predicate over the analysis results *)
let satisfy f filename p_begin p_end =
  try f (find_results filename p_begin p_end)
  with Not_found -> false

(* satisfy, but with the intersection of all analysis results. *)
let satisfy1 f =
  let inter mbR r =
    match mbR with
      None   -> None
    | Some s -> intersect_results r s in
  satisfy
    begin fun ls ->
      match ls with
        []        -> false
      | (x :: xs) ->
	  match List.fold_left inter (Some x) xs with
	    None   -> false
	  | Some r -> f r
    end

let has_any_result = satisfy (fun rs -> List.length rs > 0)

let for_all p = satisfy (List.for_all p)

let for_all1 p = satisfy
  (fun rs -> List.length rs > 0 && List.for_all p rs)

let exists p = satisfy (List.exists p)

let single_int c r =
  match r with
  | IntSet s when Int64Set.is_empty s -> true  (* unreachable memory, thus any property holds *)
  | IntSet s                   -> Int64Set.equal (Int64Set.singleton c) s
  | IntBounds (Some l, Some u) -> l == c && u == c
  | _                          -> false

let contains_int c r =
  match r with
    IntSet s         -> Int64Set.mem c s
  | IntBounds (l, u) -> within_bounds c l u
  | _                -> false

let has_only_nul = for_all1 (single_int Int64.zero)
let has_also_nul = exists (contains_int Int64.zero)
let has_also_int c = exists (contains_int c)

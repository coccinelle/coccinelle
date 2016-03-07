open Common

open Oset

open Parser_c

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * Is this module make all the tricks used in parsing_hacks and
 * most definitions in standard.h obsolete ? It depends. In a
 * static analysis context we want to be accurate, and so expand
 * all the code that will make our type/callgraph analysis simpler.
 * So we want to expand many macros, based on heuristics in this file.
 * In a transformation context, we want to let the programmer
 * match over certain constructs such as declarator, iterator,
 * macro_field, etc, and in this case we want to parse as-is.
 *
 * What could be done is that some of the analysis performed in this
 * file could then be injected in parsing_hacks, for instance via
 * hints, to make the parse as-is job easier too.
 *
 *
 *
 * todo: right now I find dangerous macro based on ## and go upward
 * to also include calling macros. But this dangerous macro itself
 * may use other macros that looks ok but that should also be expanded
 * because it defines some entities. So also recurse downward ?
 *
 * todo? do analysis a la Astec ? try infer the meaning of the macro
 * from its body but also from its context of use ? Can then
 * do a taxonomy of macro ? not just foreach or declarator but
 * polymorphic function (e.g. MAX), type generator, etc. Cf astec paper
 * or Ernst cpp study paper ?
 *
 *)

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

(* callgraph of macros *)

module Key : Set.OrderedType with type t = string = struct
  type t = string
  let compare = String.compare
end

module KeySet = Set.Make (Key)

module KeyMap = Map.Make (Key)

module Node :
  Set.OrderedType with
  type t = (Common.filename * Cpp_token_c.define_def) list ref =
struct
  type t = (Common.filename * Cpp_token_c.define_def) list ref
  let compare = compare
end

type edge = Direct

module Edge : Set.OrderedType with type t = edge =
struct
  type t = edge
  let compare = compare
end

module KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t =
struct
  type t = Key.t * Edge.t
  let compare = compare
end

module KeyEdgeSet = Set.Make (KeyEdgePair)

module G = Ograph_simple.Make
  (Key) (KeySet) (KeyMap) (Node) (Edge) (KeyEdgePair) (KeyEdgeSet)

type callgraph_macros = G.ograph_mutable

let rootname = "__ROOT__"

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
let bodytoks_of_body body =
  match body with
  | Cpp_token_c.DefineHint _ ->
      pr2 "weird, hint in cpp_analysis_c";
      []
  | Cpp_token_c.DefineBody xs ->
      xs


let build_empty_set () = new Osetb.osetb Setb.empty


(*****************************************************************************)
(* Builder  *)
(*****************************************************************************)

let build_callgraph_macros xs =
  let (g: callgraph_macros) = new G.ograph_mutable in

  g#add_node rootname (ref []);

  (* build nodes *)
  xs +> List.iter (fun (file, (x, def)) ->
    (* todo? if exist already ? *)
    g#add_node x (ref []);
    g#add_arc (rootname, x) Direct;
  );
  xs +> List.iter (fun (file, (x, def)) ->
    let node = KeyMap.find x g#nodes in
    Common.push2 (file, def) node;
  );

  (* build edges *)
  xs +> List.iter (fun (file, (x, def)) ->
    let (s, params, body) = def in
    let toks = bodytoks_of_body body in
    toks +> List.iter (fun tok ->
      match tok with
      | TIdent (x2,ii) ->
          (try
            let _ = KeyMap.find (x2 : Key.t) g#nodes in
            g#add_arc (x, x2) Direct;
          with
           Not_found -> ()
          )
      | _ ->
          ()
    );

  );
  g


(* ---------------------------------------------------------------------- *)
let check_no_loop_graph g =

  let self_referential         = ref [] in
  let macros_in_loop_with_path = ref [] in

  let already = Hashtbl.create 101 in

  let already_error_msg = Hashtbl.create 101 in

  let rec aux_dfs path xi =
    if Hashtbl.mem already xi && List.mem xi path
    then begin
      let node = KeyMap.find xi g#nodes in
      let file =
        match !node with
        | (file, _)::xs -> file
        | [] -> raise (Impossible 74)
      in
      (* in apache/srclib/apr/include/arch/win32/apr_dbg_win32_handles.h
       * we get some __ROOT__ -> CreateMutexA -> CreateMutexA because
       * the macro is self referential. Probably cpp has
       * some special handling of such case and does not expand
       * recursively.
       *
       *)
      let is_self_reference =
        match xi::path with
        | x::y::z -> x = y
        | _ -> false
      in
      if not is_self_reference && not (Hashtbl.mem already_error_msg xi)
      then begin
        Hashtbl.add already_error_msg xi true;
        pr2 (spf "PB: loop in macro %s of file %s" xi file);
        pr2 (spf "path is: %s" (String.concat " -> " (List.rev (xi::path))));
        Common.push2 (xi, path) macros_in_loop_with_path;
      end
      else begin
        Common.push2 xi self_referential;
      end
    end else begin
      Hashtbl.add already xi true;
      (* f xi path; *)
      let aux (key, _) keyset = KeySet.add key keyset in
      let succ = KeyEdgeSet.fold aux (g#successors xi) KeySet.empty in
      let aux2 yi = aux_dfs (xi::path) yi in
      KeySet.iter aux2 succ
    end
  in
  aux_dfs [] rootname;
  !self_referential, !macros_in_loop_with_path

(* ---------------------------------------------------------------------- *)
let slice_of_callgraph_macros (g: callgraph_macros) goodnodes =

  let (g': callgraph_macros) = new G.ograph_mutable in

  let f k =
    let v = KeyMap.find k g#nodes in
    g'#add_node k v in
  KeySet.iter f goodnodes;

  let f' k =
    let aux (key, _) keyset = KeySet.add key keyset in
    let succ = KeyEdgeSet.fold aux (g#successors k) KeySet.empty in
    let inter = KeySet.inter succ goodnodes in
    let f'' k' = g'#add_arc (k, k') Direct in
    KeySet.iter f'' inter in
  KeySet.iter f' goodnodes;
  g'

(*****************************************************************************)
(* Macros expansion  *)
(*****************************************************************************)

(* get the longuest one ? or the one that contains the dangerous macro ? *)
let get_single_file_and_def_of_node k v =
  match !v with
  | [] -> raise (Impossible 75)
  | [file, def] -> file, def
  | (file, def)::y::ys ->
      pr2 (spf "multiple def for %s but I kept only one" k);
      file, def

module TV = Token_views_c

let (macro_expand:
     (string, Cpp_token_c.define_def) Hashtbl.t ->
      Cpp_token_c.define_def -> Cpp_token_c.define_def) =
 fun current_def def ->
  let (s, params, body) = def in
  let body' =
    match body with
    | Cpp_token_c.DefineHint _ ->
        body
    | Cpp_token_c.DefineBody xs ->
        (* bugfix: we dont want to evalute the x ## b at this moment.
         * so can not use fix_tokens_cpp in the same we use it
         * to parse C code.
        let xs' =
          Parsing_hacks.fix_tokens_cpp ~macro_defs:current_def xs
        in
         *)
        let tokens = xs in
        let tokens2 = ref (tokens +> Common.acc_map TV.mk_token_extended) in
        let cleaner = !tokens2 +> Parsing_hacks.filter_cpp_stuff in
        let paren_grouped = TV.mk_parenthised  cleaner in
        Cpp_token_c.apply_macro_defs
          ~msg_apply_known_macro:(fun s2 ->
            pr2 (spf "APPLYING: %s in definition of %s" s2 s))
          ~msg_apply_known_macro_hint:(fun s ->
            pr2 "hint")
          ~evaluate_concatop:false
          ~inplace_when_single:false
          current_def paren_grouped;
        (* because the before field is used by apply_macro_defs *)
        tokens2 := TV.rebuild_tokens_extented !tokens2;

        (* bugfix *)
        let cleaner = !tokens2 +> Parsing_hacks.filter_cpp_stuff in

        let xs' =
          Parsing_hacks.insert_virtual_positions
            (cleaner +> Common.acc_map (fun x -> x.TV.tok))
        in

        Cpp_token_c.DefineBody xs'
  in
  (s, params, body')


(* work by side effect as both the binding and callgraph are mutable
 * data structure
 *)
let no_inlining = ref false

let rec (recurse_expand_macro_topological_order:
         int -> (string, Cpp_token_c.define_def) Hashtbl.t ->
         callgraph_macros -> unit) =
 fun depth current_def g ->

  (* naive: *)
  if !no_inlining then
    let f k v =
      if k = rootname then ()
      else
        let def = get_single_file_and_def_of_node k v +> snd in
        Hashtbl.add current_def k def in
    KeyMap.iter f g#nodes

  else
    let remaining = KeyMap.bindings g#nodes in
    (match remaining with
    | [] -> () (* christia: commented this out: raise (Impossible 76)
		* This seems to be the case when there are no
		* problematic macros. Which is possible.
		*)
    | [(k,n)] ->
        assert (k = rootname);
        (* end recursion *)
        ()
    | (k, v)::y::xs ->
        let leafs = KeySet.elements (g#leaf_nodes ()) in
        pr2 (spf "step: %d, %s" depth (leafs +> String.concat " "));

        G.print_ograph_generic
          ~str_of_key:(fun k -> k)
          ~str_of_node:(fun k node -> k)
          (spf "/tmp/graph-%d.dot" depth)
          g;

        assert (leafs <> []);


        (* little specialisation to avoid useless work *)
        if depth = 0
        then begin
          leafs +> List.iter (fun k ->
            let node = KeyMap.find k g#nodes in
            let def = get_single_file_and_def_of_node k node +> snd in
            Hashtbl.add current_def k def
          )
        end else begin
          let new_defs =
            leafs +> List.map (fun k ->
              let node = KeyMap.find k g#nodes in
              let def = get_single_file_and_def_of_node k node +> snd in
              let def' = macro_expand current_def def in
              k, def'
            )
          in
          new_defs +> List.iter (fun (k,def) -> Hashtbl.add current_def k def);
        end;
        leafs +> List.iter (fun k -> g#del_leaf_node_and_its_edges k);
        recurse_expand_macro_topological_order (depth+1) current_def g;
    )



(*****************************************************************************)
(* Macros def analysis  *)
(*****************************************************************************)

let is_dangerous_macro def =
  let (s, params, body) = def in
  let toks = bodytoks_of_body body in

  (match params, body with

  (* ex: APU_DECLARE_DATA *)
  | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [] ->
      if s =~ ".*_H_*"
      then false
      else true

  (* ex: AP_DECLARE(x) x  *)
  | Cpp_token_c.Params([s1]),
	Cpp_token_c.DefineBody [TIdent (s2,i1)] ->
	  (match s1 with
	    Cpp_token_c.FixedArg s1 -> s1 = s2
	  | Cpp_token_c.VariadicArg _ -> false)

  (* keyword aliases. eg: APR_inline __inline__ *)
  | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [x] ->
      (match x with
      | Tinline _ -> true
      | Tconst _ -> true
      | Tstatic _ -> true
      | Textern _ -> true
      | _ -> false
      )

  | _ , Cpp_token_c.DefineBody xs ->
      (match List.rev xs with
      (* make extract_macros looping on apache, get some infinite "step x" *)
      | TPtVirg _::_ -> true
      | _ -> false
      )

  | _ -> false
  ) ||


  (toks +> List.exists (fun tok ->
    match tok with
    | TCppConcatOp _ -> true

    | Tattribute (ii) -> true
    | TattributeNoarg (ii) -> true

(* FP with local variable.
    | TIdent (s,ii) ->
        s ==~ Parsing_hacks.regexp_annot && not (List.mem s
          ["__FILE__";"__LINE__";"__FUNCTION__"])
*)
    | _ -> false
  ))


let is_trivial_macro def =
  let (s, params, body) = def in
  match params, body with
  | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [Parser_c.TInt _]
(* no!!! those are not trivial macro, they are dangerous too.
  | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [] ->
      true
*)
  | _ ->
      false

(*
            | () when s ==~ Parsing_hacks.regexp_annot -> true
            | () when List.exists (function
                 (*| Parser_c.Tattribute _ -> true*)
                 | Parser_c.TCppConcatOp _ -> true
                 | _ -> false) bodytoks
               -> true
            | () -> false
          in
*)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let extract_dangerous_macros xs =

  (* prepare initial set of macro definitions to work on *)
  let all_macros =
    xs +> List.map (fun (file, defs) ->
      defs +> List.map (fun def -> file, def)
    ) +> List.flatten
  in
  let macros =
    all_macros +> Common.exclude(fun (file,(x,def)) -> is_trivial_macro def) in

  (* initial set of problematic macros *)
  let problematic_macros =
    macros +> Common.filter (fun (file, (x, def)) -> is_dangerous_macro def) in


  (* include the ancestors of problematic macros *)
  let g =
    build_callgraph_macros macros in
  let self_referiential, macros_in_loop_with_path =
    check_no_loop_graph g in

  G.print_ograph_generic
    ~str_of_key:(fun k -> k)
    ~str_of_node:(fun k node -> k)
    "/tmp/graph.dot"
    g;
  let start =
    problematic_macros +> List.map (fun (file, (x, def)) -> x) +> Common.nub in

  let finalset =
    start +> List.fold_left (fun acc x ->
      if List.exists (fun y -> fst y = x) macros_in_loop_with_path
        || List.mem x self_referiential
      then begin
        pr2 (spf "PB: ignoring %s macro as it is in a loop" x);
        acc
        end
      else
        let acc = KeySet.add x acc in
        let ancestors = g#ancestors x in
        KeySet.union acc ancestors
    ) KeySet.empty
  in

  (* Now prepare for fixpoint expansion of macros to avoid doing
   * the work in cpp_engine.
   *)
  let sliced_g =
    slice_of_callgraph_macros g finalset
  in
  G.print_ograph_generic
    ~str_of_key:(fun k -> k)
    ~str_of_node:(fun k node -> k)
    "/tmp/graph2.dot"
    sliced_g;


  (* do fixpoint expansion *)
  let (binding: (string, Cpp_token_c.define_def) Hashtbl.t) =
    Hashtbl.create 101 in
  (* work by side effects on the hashtbl and graph *)
  recurse_expand_macro_topological_order 0 binding sliced_g;



  (* prepare final result *)
  let final_macros =
    binding +> Common.hash_to_list +> List.map (fun (x, def) ->
      let node = KeyMap.find x g#nodes in
      let file = get_single_file_and_def_of_node x node +> fst in
      (file, (x, def))
    )
  in

  pr2 (spf "total macros numbers: %d"
          (List.length all_macros));
  pr2 (spf "problematic macros numbers: %d"
          (List.length problematic_macros));
  pr2 (spf "final (after closure) problematic macros numbers: %d"
          (List.length final_macros));

  let grouped = Common.group_assoc_bykey_eff final_macros in
  grouped

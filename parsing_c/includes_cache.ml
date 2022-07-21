(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common
module Lib = Lib_parsing_c

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr_inc s =
  if !Flag_parsing_c.verbose_includes
  then Common.pr2 s

(*****************************************************************************)
(* Graph types/modules *)
(*****************************************************************************)

(* Filenames as keys to check paths from file A to file B. *)
module Key : Set.OrderedType with type t = Common.filename = struct
  type t = Common.filename
  let compare = String.compare
end

module KeySet = Set.Make (Key)

module KeyMap = Map.Make (Key)

module Node : Set.OrderedType with type t = unit = struct
  type t = unit
  let compare = compare
end

module Edge : Set.OrderedType with type t = unit = struct
  type t = unit
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


(*****************************************************************************)
(* Includes dependency graph *)
(*****************************************************************************)

(* Header file includes dependency graph *)
let dependency_graph = ref (new G.ograph_mutable)

(* Check if a path exists between one node to another.
 * Almost a copy of dfs_iter in commons/ograph_extended.ml with minor changes.
 * Return true if g satisfies predicate f else false
 *)
let dfs_exists xi f g =
  let already = Hashtbl.create 101 in
  let rec aux_dfs xs =
    let h xi =
      if Hashtbl.mem already xi
      then false
      else
        begin
          Hashtbl.add already xi true;
          if f xi
          then true
          else
            begin
              let f' (key, _) keyset = KeySet.add key keyset in
              let newset =
                try KeyEdgeSet.fold f' (g#successors xi) KeySet.empty
                with Not_found -> KeySet.empty in
              aux_dfs newset
            end
        end in
    KeySet.exists h xs in
  aux_dfs (KeySet.singleton xi)

let add_to_dependency_graph parent file =
  let add_node a =
    if not (KeyMap.mem a !dependency_graph#nodes)
    then !dependency_graph#add_node a () in
  let add_arc (a, b) =
    add_node a;
    add_node b;
    !dependency_graph#add_arc (a, b) () in
  add_arc (parent, file)

let path_exists_dependency_graph filea fileb =
  dfs_exists filea (fun a -> a = fileb) !dependency_graph

let print_dependency_graph _ =
  G.print_ograph_generic
    ~str_of_key:(fun k -> "\"" ^ k ^ "\"")
    ~str_of_node:(fun k _ -> k)
    "/tmp/headers.dot"
    !dependency_graph


(*****************************************************************************)
(* Name cache *)
(*****************************************************************************)

type cache_exp =
  | CacheField of string (* Name of the struct/union it is defined in *)
  | CacheEnumConst
  | CacheVarFunc
  | CacheTypedef

(* Not very elegant. Basically a copy-paste of namedef in type_annoter_c. *)
type cache_return =
  | RetVarOrFunc of string * Ast_c.exp_type
  | RetEnumConstant of string * Ast_c.fullType
  | RetTypeDef   of string * Ast_c.fullType
  | RetStructUnionNameDef of string * (Ast_c.structUnion * Ast_c.structType)
                          Ast_c.wrap

(* Map name to list of files it is defined in *)
let name_cache : (string, (filename * cache_exp) list) Hashtbl.t ref =
  ref (Hashtbl.create 101)

let add_to_name_cache name (file, exp) =
  let l =
    try Hashtbl.find !name_cache name
    with Not_found -> [] in
  Hashtbl.replace !name_cache name ([(file, exp)] @ l)

(* Visitor to cache names in a given file. *)
let cache_name_visitor file =

  let cache_struct_fields sname def =
    let _cache_field field =
      match (Ast_c.unwrap field) with
        Ast_c.Simple (name, _)
      | Ast_c.BitField (name, _, _, _) ->
          name +>
            do_option
              (fun n ->
                 add_to_name_cache (Ast_c.str_of_name n)
                 (file, CacheField(sname))) in
    let _cache_struct_fields field =
      match field with
        Ast_c.DeclarationField(Ast_c.FieldDeclList(l)) ->
          List.iter _cache_field (Ast_c.unwrap l)
      | _ -> () in
    List.iter _cache_struct_fields def in

  let cache_enum_constants def =
    def +>
      List.iter
        (fun ec ->
           add_to_name_cache
             ((Ast_c.unwrap ec) +> fst +> Ast_c.str_of_name)
             (file, CacheEnumConst)) in

  { Visitor_c.default_visitor_c with
    Visitor_c.ktoplevel = fun (k, bigf) p ->
      match p with
        Ast_c.Declaration
          (Ast_c.DeclList (defs, _)) ->
             let cache_name x =
               (match (Ast_c.unwrap x) with
                 {Ast_c.v_namei = Some (name, _);
                  Ast_c.v_type = typ;
                  Ast_c.v_storage = strg} ->
                  (* Cache typedefs/variables/functions *)
                    let exp_type =
                      match (fst strg) with
                        Ast_c.StoTypedef -> CacheTypedef
                      | _ -> CacheVarFunc in
                    add_to_name_cache
                      (Ast_c.str_of_name name) (file, exp_type)
               | {Ast_c.v_namei = None; Ast_c.v_type = typ} ->
                    (match (Ast_c.unwrap (snd typ)) with
                      Ast_c.StructUnion (_, Some n, _, def) ->
                        (* Cache field names *)
                        cache_struct_fields n def
                    | Ast_c.EnumDef(_, base, def) ->
                        (* Cache enumeration constants *)
                        cache_enum_constants def
                    | _ -> k p)) in
             List.iter cache_name defs
      | _ -> k p
  }


let get_type_visitor file l =
  let add_to_ret ret =
    l := [ret] @ !l in
  let get_enum_constants sopt base def =
    def +>
      List.iter
        (fun ec ->
           let s = (Ast_c.unwrap ec) +> fst +> Ast_c.str_of_name in
           add_to_ret (RetEnumConstant(s, sopt))) in
  { Visitor_c.default_visitor_c with
    Visitor_c.ktoplevel = fun (k, bigf) p ->
      match p with
        Ast_c.Declaration
          (Ast_c.DeclList (defs, _)) ->
             let get_name x =
               (match (Ast_c.unwrap x) with
                 {Ast_c.v_namei = Some (n, _);
                  Ast_c.v_type = typ;
                  Ast_c.v_storage = strg} ->
                  (* Cache typedefs/variables/functions *)
                    let f _ =
                      let s = Ast_c.str_of_name n in
                      match (fst strg) with
                        Ast_c.StoTypedef ->
                          add_to_ret (RetTypeDef(s, typ))
                      | Ast_c.NoSto
                      | Ast_c.Sto _ ->
                          add_to_ret
                            (RetVarOrFunc(s, (typ, Ast_c.NotLocalVar))) in
                    f ()
               | {Ast_c.v_namei = None; Ast_c.v_type = typ} ->
                   (match (Ast_c.unwrap (snd typ)) with
                     Ast_c.StructUnion (su, snameopt, _, def) ->
                       (match snameopt with
                         Some s ->
                           let def' = Lib.al_fields def in
                           let ii = Ast_c.get_ii_typeC_take_care (snd typ) in
                           let ii' = Lib.al_ii ii in
                           add_to_ret
                             (RetStructUnionNameDef (s, ((su, def'),ii')))
                       | None -> k p)
                   | Ast_c.EnumDef(sopt, base, def) ->
                       get_enum_constants sopt base def
                   | _ -> k p)) in
             List.iter get_name defs
      | _ -> k p
  }

let extract_names file ast =
  ignore (Visitor_c.vk_program (cache_name_visitor file) ast)

(* Use the visitor to get all the types from a file. *)
let get_types_from_name_cache file name exp_types parse_fn =
  if !Includes.include_headers_for_types && Includes.is_header file
  then []
  else
    let rec find_file l =
      match l with
        [] -> None
      | (x, y)::xs ->
          if List.mem y exp_types
          then
            begin
              if path_exists_dependency_graph file x
              then Some (x, y)
              else find_file xs
            end
          else find_file xs in
    let file_list =
      try Hashtbl.find !name_cache name
      with Not_found -> [] in
    match (find_file file_list) with
      None -> []
    | Some (x, t) ->
        let ast = parse_fn x in
        let env = ref [] in
        ignore
          (Visitor_c.vk_program
            (get_type_visitor file env) ast);
        !env


(*****************************************************************************)
(* Set of parsed files *)
(*****************************************************************************)

module StringSet = Set.Make (String)

(* Set of files parsed *)
let seen_files = ref (StringSet.empty)

let has_been_parsed file =
  StringSet.mem file !seen_files

let add_to_parsed_files file =
  seen_files := StringSet.add file !seen_files

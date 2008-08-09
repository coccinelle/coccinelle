open Common

open Ast_c 


(* used both for storing the entities 'defined' and 'used' in a file, but
 * depending on the use, some fields may not be used
 *)
type entities = {
  macros: string hashset; (* object-like or function-like *)
  variables: string hashset;
  static_variables: string hashset; (* for defined only *)
  functions: string hashset;
  static_functions: string hashset; (* for defined only *)
  structs: string hashset; (* union defs, enum defs, enum values *)
  typedefs: string hashset;
  include_c: filename hashset; (* for used only *)
}

(* inverted index *)
type idx_entities = { 
  idx_macros: (string, (filename hashset)) Hashtbl.t; 
  idx_variables: (string, (filename hashset)) Hashtbl.t; 
  idx_functions: (string, (filename hashset)) Hashtbl.t; 
  idx_structs: (string, (filename hashset)) Hashtbl.t; 
  idx_typedefs: (string, (filename hashset)) Hashtbl.t; 
}

let empty_entities () = {
  macros = Hashtbl.create 101;
  variables = Hashtbl.create 101;
  static_variables = Hashtbl.create 101;
  functions = Hashtbl.create 101;
  static_functions = Hashtbl.create 101;
  structs = Hashtbl.create 101;
  typedefs = Hashtbl.create 101;
  include_c = Hashtbl.create 101;
}

let empty_idx_entities () = {
  idx_macros = Hashtbl.create 101;
  idx_variables = Hashtbl.create 101;
  idx_functions = Hashtbl.create 101;
  idx_structs = Hashtbl.create 101;
  idx_typedefs = Hashtbl.create 101;
}


let h_to_l h = Common.hashset_to_list h

let print_entities e =
 begin
(*  e.macros +> h_to_l +> List.iter (fun s -> pr("MACRO: " ^ s)); *)
  e.variables +> h_to_l +> List.iter (fun s -> pr("VAR: " ^ s));
  e.static_variables +> h_to_l +> List.iter (fun s -> pr("STATICVAR: " ^ s));
  e.functions +> h_to_l +> List.iter (fun s -> pr("FUNC: " ^ s));
  e.static_functions +> h_to_l +> List.iter (fun s -> pr("STATICFUNC: " ^ s));
  e.structs +> h_to_l +> List.iter (fun s -> pr("STRUCT: "^s));
  e.typedefs +> h_to_l +> List.iter (fun s -> pr("TYPEDEF: "^s));
  e.include_c +> h_to_l +> List.iter (fun s -> pr("INCLUDEC: "^s));
 end


(* the defined_stuff and used_stuff may not be 100% correct. They don't handle
 * I think every dark corner of the C. It's possible to shadow in so many
 * ways.
 *)
  
(* look only for toplevel definition *)
let defined_stuff xs = 
  let e = empty_entities() in
  let add h s = Hashtbl.add h  s true in

  (* look only for toplevel definition: don't recurse, don't call k *)
  let bigf = { Visitor_c.default_visitor_c with
    Visitor_c.ktoplevel = (fun (k,bigf) t -> 
      match t with
      | Declaration decl -> 
          (match decl with
          | DeclList (xs, ii) -> 
              xs +> List.iter (fun ((var, t, sto, _local), iicomma) -> 
                Visitor_c.vk_type bigf t;
                match var, sto with 
                | None, _ -> ()
                | Some ((s, ini), ii_s_ini),  (StoTypedef,inline) -> 
                    add e.typedefs s;
                | Some ((s, ini), ii_s_ini),  (Sto Static,inline) -> 
                    (* need add them to do the adjust_need *)
                    add e.static_variables s;

                | Some ((s, ini), ii_s_ini),  (Sto Extern,inline) -> 
                    ()
                | Some ((s, ini), ii_s_ini),  (_,inline) -> 
                    add e.variables s;
              );
          | MacroDecl ((s, args),ii) -> ()
          )

      | Definition def -> 
          let ((s, typ, sto, cp), ii) = def in
          (match sto with
          | Sto Static, inline ->
              (* need add them to do the adjust_need *)
              add e.static_functions s
          | _ -> 
              add e.functions s
          )

      | Include includ -> ()
      | Define ((s,ii), body) -> add e.macros s
      | MacroTop (s, args, ii) -> ()

      | EmptyDef _ | NotParsedCorrectly _ | FinalDef _ -> ()
    );

    Visitor_c.ktype = (fun (k, bigf) t -> 
      match Ast_c.unwrap_typeC t with
      | StructUnion (su, sopt, fields) -> 
          sopt +> do_option (fun s -> 
            add e.structs s;
          );
        
      | _ -> ()
    );

  } in
  xs +> List.iter (fun (p, info_item) -> Visitor_c.vk_toplevel bigf p);
  e








(* look only for use of external stuff. Don't consider local vars, 
 * typedefs, structures *)
let used_stuff xs = 

  let e = empty_entities() in
  let add h s = Hashtbl.replace h s true in

  let initial_env = [
    ["NULL";
     "do_gettimeofday";
     "le32_to_cpu";
     "udelay";
     "printk";
     (* !!! sometimes considered as VAR :( *)
     "u8"; "u16"; "u32"; 
     "s32";
    ] +> List.map (fun s -> s, true);
  ]
  in
  let regexp_macro =  Str.regexp
    "^[A-Z_][A-Z_0-9]*$" 
  in

  let (_env: (string, bool) Common.scoped_env ref) = ref initial_env in


  let bigf = { Visitor_c.default_visitor_c with

    (* --------- handling scope of variables (and also some use) --------- *)
    Visitor_c.kstatement = (fun (k, bigf) st -> 
      match st with 
      | Compound statxs, ii -> Common.do_in_new_scope _env (fun () -> k st);
      | _ -> k st
    );
    Visitor_c.kdecl = (fun (k, bigf) d -> 
      k d; (* to add possible definition in type found in Decl *)
      (match d with
      | (DeclList (xs, ii)) -> 
          xs +> List.iter (fun ((var, t, sto, _local), iicomma) -> 
            var +> do_option (fun ((s, ini), ii_s_ini) -> 
              match sto with 
              | StoTypedef, _inline -> 
                  (* add_binding (TypeDef (s)) true; *)
                  ()
              | _ ->
                  Common.add_in_scope _env (s, true);
            );
          );
      | _ -> ()
      );
    );
    Visitor_c.ktoplevel = (fun (k, bigf) elem -> 
      match elem with
      | Definition def -> 
          let (funcs, ((returnt, (paramst, b))), sto, statxs),ii = def in
          Common.do_in_new_scope _env (fun () -> 
            paramst +> List.iter (fun (((b, s, t), _),_) -> 
              match s with 
              | Some s -> Common.add_in_scope _env (s, true)
              | None -> pr2 "no type, certainly because Void type ?"
            );
            k elem
          );
      | Define (s, (defkind, defval)) -> 
          Common.do_in_new_scope _env (fun () -> 
            (match defkind with
            | DefineFunc (params, ii) -> 
                params +> List.iter (fun ((s,iis), iicomma) -> 
                  Common.add_in_scope _env (s, true)
                );
            | _ -> ()
            );
            k elem
          );
      | Include ((inc_file,ii), posref) -> 
          (match inc_file with
          | Local [x] when x =~ ".*\\.c$" -> 
              add e.include_c x
          | _ -> ()
          )
      | _ -> k elem
    );


    (* --------- and now looking for use --------- *)
    Visitor_c.kexpr = (fun (k,bigf) x -> 
      match Ast_c.unwrap_expr x with

      | FunCall (((Ident f, typ), ii), args) -> 
          if not (Common.member_env_key f !_env)
          then 
            if f ==~ regexp_macro
            then add e.macros f
            else add e.functions f
          ;
          args +> List.iter (fun (x,ii) -> 
            Visitor_c.vk_argument bigf x
          );
      | Ident s -> 
          if not (Common.member_env_key s !_env)
          then 
            if s ==~ regexp_macro
            then add e.macros s
            else add e.variables s

      | _ -> k x
    );

    Visitor_c.ktype = (fun (k,bigf) t -> 
      match Ast_c.unwrap_typeC t with
      | StructUnionName (su, s) -> 
          if not (Common.member_env_key s !_env)
          then 
            add e.structs s;
      | TypeName (s,_typ) -> 
          if not (Common.member_env_key s !_env)
          then 
            add e.typedefs s;
      | _ -> k t
    );

  } in
  xs +> List.iter (fun (p, info_item) -> Visitor_c.vk_toplevel bigf p);
  e




(* for the moment, just look if it looks like a linux module file *)
let extra_stuff xs = 
  let is_module = ref false in

  (* look only for toplevel definition: don't recurse, don't call k *)
  let bigf = { Visitor_c.default_visitor_c with
    Visitor_c.ktoplevel = (fun (k,bigf) t -> 
      match t with
      | MacroTop (s, args, ii) -> 
          if s = "module_init"
          then is_module := true;
      | Definition def -> 
          let ((s, typ, sto, cp), ii) = def in
          if s = "init_module" 
          then is_module := true;

      | _ -> ()
    );
  } in
  xs +> List.iter (fun (p, info_item) -> Visitor_c.vk_toplevel bigf p);
  !is_module



let adjust_used_only_external used defined = 
 begin
  used.variables +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.variables s || 
       Hashtbl.mem defined.static_variables s || 
       (* sometimes functions are used as variable, when for example 
        * stored in a pointer variable, so look also for function here.
        *)
       Hashtbl.mem defined.functions s ||
       Hashtbl.mem defined.static_functions s
    then Hashtbl.remove used.variables s
  );
  used.functions +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.functions s || 
       Hashtbl.mem defined.static_functions s
    then Hashtbl.remove used.functions s
  );
  used.structs +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.structs s
    then Hashtbl.remove used.structs s
  );
  used.typedefs +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.typedefs s
    then Hashtbl.remove used.typedefs s
  );
 end


 

type file_info = { 
  used: entities;
  defined: entities;
  is_module: bool;
}
type global_definitions = idx_entities

let mk_global_definitions_index xs = 
  let idx = empty_idx_entities () in
  xs +> List.iter (fun (file, {defined = defined}) -> 
    defined.variables +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_variables;
    );
    defined.functions +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_functions;
    );
    defined.structs +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_structs;
    );
    defined.typedefs +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_typedefs;
    );
  );
  idx

let known_duplicate = 
  ["init_module"; "cleanup_module";
   "main";"usage";
  ] 

let check_no_duplicate_global_definitions idx = 
 begin
  pr2 "DUPLICATE processing:";
  idx.idx_functions +> hash_to_list +> List.iter (fun (f, set) -> 
    let xs = hash_to_list set in
    if List.length xs <> 1 && not (List.mem f known_duplicate)
    then 
      pr2 ("multiple def for : " ^ f ^ " in " ^ 
              (join " " (List.map (fun x -> basename (fst x)) xs)));
  );
 end
  
type dependencies_graph = 
 ((filename * file_info) * string, bool) Ograph_extended.ograph_mutable


let build_graph xs dep graphfile = 
  let g = ref (new Ograph_extended.ograph_mutable)  in
  let h = Hashtbl.create 101 in
  let s_to_nodei s = Hashtbl.find h s in

  pr2 "BUILDING graph:";

  let arrow a b c d = 
    (sprintf "%-20s -- %s:%25s --> %s" a b c d) 
  in
  with_open_outfile (graphfile ^ ".graph") (fun (pr_no_nl,chan) -> 
    let pr_arrow a b c d = 
      pr2 (arrow a b c d);
      pr_no_nl (arrow a b c d  ^ "\n");
    in

  (* build nodes *)
  xs +> List.iter (fun (file, cinfo) -> 
    let s = (if cinfo.is_module then "[M]" else "") ^ Filename.basename file in
    let xi = !g#add_node ((file, cinfo), s) in
    Hashtbl.add h file xi;
  );

  xs +> List.iter (fun (file, {used = used}) -> 

    used.functions +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_functions s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
            let (file, file_defined) = basename file, basename file_defined in
            pr_arrow file "f" s file_defined;
          );
    );
    (* sometime use functions as variable *)
    used.variables +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_functions s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
            let (file, file_defined) = basename file, basename file_defined in
            pr_arrow file "f" s file_defined;
          );
    );

    used.variables +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_variables s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
            let (file, file_defined) = basename file, basename file_defined in
            pr_arrow file "v" s file_defined;
          );
    );

    used.include_c +> h_to_l +> List.iter (fun local_file -> 
      let file_defined = Filename.concat (dirname file) local_file in
      try (
        !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
        let (file, file_defined) = basename file, basename file_defined in
        pr_arrow file "I" "include" file_defined;
      )
      with Not_found -> 
        pr2 ("can't find included C file: " ^ file_defined)
    )

(*
    used.structs +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_structs s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
          );
    );

    used.typedefs +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_typedefs s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
          );
    );
*)
  );
  );
  Ograph_extended.print_ograph_mutable !g graphfile (!Flag.show_misc);
  !g





let generate_makefile (g: dependencies_graph) file = 
  pr2 "GENERATING makefile";
  with_open_outfile file (fun (pr_no_nl, chan) ->

  let nodei_to_file xi = 
    let ((file, cinfo ), s) = (g#nodes#assoc xi) in
    file
  in
  let all_nodes = g#nodes#tolist +> List.map fst in
  let visited_nodes_h = Hashtbl.create 101 in

  let modules = all_nodes +> List.filter (fun xi -> 
    let ((file, cinfo), s) = g#nodes#assoc xi in
    cinfo.is_module
  ) in
  
  pr_no_nl "  # ---- modules files ---- \n";
  modules +> List.iter (fun xi -> 
    pr2 (nodei_to_file xi);
    pr_no_nl " ";
    g +> Ograph_extended.dfs_iter xi (fun yi -> 
      pr2 ("   " ^ (Filename.basename (nodei_to_file yi)));
      pr_no_nl (" " ^ (Filename.basename (nodei_to_file yi)));
      Hashtbl.add visited_nodes_h yi true;
    );
    pr_no_nl "\n";
  );
  let visited_nodes = Common.hashset_to_list visited_nodes_h in
  let rest = all_nodes $-$ visited_nodes in

  let startfiles = rest +> List.filter (fun xi -> 
    (g#predecessors xi)#null
  ) in
  pr_no_nl "  # ---- not module starting files ---- \n";

  startfiles +> List.iter (fun xi -> 
    pr2 (nodei_to_file xi);
    pr_no_nl " ";
    g +> Ograph_extended.dfs_iter xi (fun yi -> 
      pr2 ("   " ^ (Filename.basename (nodei_to_file yi)));
      pr_no_nl (" " ^ (Filename.basename (nodei_to_file yi)));
      Hashtbl.add visited_nodes_h yi true;
    );
    pr_no_nl "\n";
  );
  let visited_nodes = Common.hashset_to_list visited_nodes_h in
  let rest = rest $-$ visited_nodes in
  
  if not (null rest) then pr_no_nl "  # ---- files in cycle ---- \n";
  rest +> List.iter (fun xi -> 
    if Hashtbl.mem visited_nodes_h xi then () (* already handled *)
    else begin
      pr2 (nodei_to_file xi);
      pr_no_nl " ";
      g +> Ograph_extended.dfs_iter xi (fun yi -> 
        pr2 ("   " ^ (Filename.basename (nodei_to_file yi)));
        pr_no_nl (" " ^ (Filename.basename (nodei_to_file yi)));
        Hashtbl.add visited_nodes_h yi true;
      );
      pr_no_nl "\n";
    end
  )
  )


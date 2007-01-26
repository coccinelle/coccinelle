open Common open Commonop

open Ast_c

module Lib = Lib_parsing_c

(*****************************************************************************)
(* can either:
 *
 *  - do a kind of inferer
 *     * can first do a simple inferer, that just pass context
 *     * then a real inferer, managing partial info.
 *    type context = fullType option
 *
 *  - extract the information from the .h files
 *   (so no inference at all needed)
 * 
 * todo: expression contain types, and statements,   which in turn can contain
 * expression, so need recurse. Need define an annote_statement and 
 * annotate_type.

 * todo: how deal with typedef isomorphisms ? How store them in Ast_c ?
 * store all posible variations in ast_c ? a list of type instead of just
 * the type ?
 * 
 * define a new type ? like type_cocci ? where have a bool ?
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let pr2 s = 
  if !Flag_parsing_c.verbose_type
  then Common.pr2 s

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* the different namespaces from stdC manual:
 * 
 * You introduce two new name spaces with every block that you write.
 * One name space includes all functions, objects, type definitions, and
 * enumeration constants that you declare or define within the block.
 * The other name space includes all enumeration, structure, and union
 * tags that you define within the block.

 * You introduce a new member name space with every structure or union
 * whose content you define. You identify a member name space by the
 * type of left operand that you write for a member selection operator,
 * as in x.y or p->y. A member name space ends with the end of the block
 * in which you declare it.

 * You introduce a new goto label name space with every function
 * definition you write. Each goto label name space ends with its
 * function definition.
 *)

(* But I don't try to do a type-checker, I try to "resolve" type of var
 * so don't need make difference between namespaces here *)

type namedef = 
  | VarOrFunc of string * fullType
  | TypeDef of string * fullType
  (* todo: EnumConstant *)

  | StructUnionNameDef of string * structType
  (* todo: EnumDef *)

type environment = namedef list list (* cos have nested scope, so nested list*)

let initial_env = [[]]


let rec lookup_env f env = 
  match env with 
  | [] -> raise Not_found
  | []::zs -> lookup_env f zs
  | (x::xs)::zs -> 
      match f x with
      | None -> lookup_env f (xs::zs)
      | Some y -> y, xs::zs

    

let lookup_var s env = 
  let f = function
    | VarOrFunc (s2, typ) -> if s2 = s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_typedef s env = 
  let f = function
    | TypeDef (s2, typ) -> if s2 = s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_structunion (s, su) env =
  let f = function
    | StructUnionNameDef (s2, typ) -> if s2 = s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let member_env lookupf env = 
  try 
    let _ = lookupf env in
    true
  with Not_found -> false

(*****************************************************************************)
(* "type-lookup"  *)
(*****************************************************************************)

(* Because in C one can redefine in nested blocks some typedefs,
 * struct, or variables, we have a static scoping resolving process.
 * So, when we look for the type of a var, if this var is in an
 * enclosing block, then maybe its type refer to a typdef of this
 * enclosing block, so must restart the "type-resolving" of this
 * typedef from this enclosing block, not from the bottom. So our
 * "resolving-type functions" take an env and also return an env from
 * where the next search must be performed. *)

(* do pointfix *)
let rec find_final_type ty env = 

  match Ast_c.unwrap_typeC ty with 
  | BaseType x  -> (BaseType x) +> Ast_c.rewrap_typeC ty
      
  | Pointer t -> (Pointer (find_final_type t env))  +> Ast_c.rewrap_typeC ty
  | Array (e, t) -> Array (e, find_final_type t env) +> Ast_c.rewrap_typeC ty
      
  | StructUnion (sopt, su) -> StructUnion (sopt, su)  +> Ast_c.rewrap_typeC ty
      
  | FunctionType t -> (FunctionType t) (* todo ? *) +> Ast_c.rewrap_typeC ty
  | Enum  (s, enumt) -> (Enum  (s, enumt)) (* todo? *) +> Ast_c.rewrap_typeC ty
  | EnumName s -> (EnumName s) (* todo? *) +> Ast_c.rewrap_typeC ty
      
  | StructUnionName (s, su) -> 
      let (structtyp, env') = lookup_structunion (s, su) env in
      StructUnion (Some s, structtyp) +> Ast_c.rewrap_typeC ty
        (* not wrap with good ii, but don't care *)
      
  | TypeName s -> 
      let (t', env') = lookup_typedef s env in
      find_final_type t' env'
      
  | ParenType t -> find_final_type t env
  

let (find_type_field: string -> Ast_c.structType -> Ast_c.fullType) = 
  fun fld (su, fields) -> 
    fields +> Common.find_some (fun x -> 
      match Ast_c.unwrap x with
      | FieldDeclList onefield_multivars -> 
          Common.optionise (fun () -> 
            onefield_multivars +> Common.find_some (fun fieldkind -> 
              match Ast_c.unwrap (Ast_c.unwrap fieldkind) with
              | Simple (Some s, t) | BitField (Some s, t, _) -> 
                  if s = fld then Some t else None
              | _ -> None
            )
          )
    )
             
  
  

(*****************************************************************************)
(* (Semi) Globals, Julia's style *)
(*****************************************************************************)

(* opti: cache ? use hash ? *)

let _scoped_env = ref initial_env

(* memoise unnanoted var, to avoid too much warning messages *)
let _notyped_var = ref (Hashtbl.create 100)

let new_scope() = _scoped_env := []::!_scoped_env 
let del_scope() = _scoped_env := List.tl !_scoped_env

let do_in_new_scope f = 
  begin
    new_scope();
    let res = f() in
    del_scope();
    res
  end

let add_env namedef =
  let (current, older) = Common.uncons !_scoped_env in
  _scoped_env := (namedef::current)::older
  
  


(* the warning argument is here to allow some binding to overwrite an 
 * existing one. With function, we first have the protype and then the def
 * and the def binding the same string is not an error.
 * todo?: but if we define two times the same function, then we will not
 * detect it :( would require to make a diff between adding a binding 
 * from a prototype and from a definition.
 *)
let add_binding namedef warning = 
  let (current, older) = Common.uncons !_scoped_env in

  (match namedef with
  | VarOrFunc (s, typ) -> 
      if Hashtbl.mem !_notyped_var s
      then pr2 ("warning: found typing information for a variable that was" ^
                   "previously unknown:" ^ s);
  | _ -> ()
  );

  let (memberf, s) = 
    (match namedef with
    | VarOrFunc (s, typ) -> member_env (lookup_var s), s
    | TypeDef (s, typ) -> member_env (lookup_typedef s), s
    | StructUnionNameDef (s, (su, typ)) -> 
        member_env (lookup_structunion (s, su)), s
    ) in

  if  memberf [current] && warning
  then pr2 ("Type_annoter: warning, " ^ s ^ 
            " is already in current binding" ^ "\n" ^
           " so there is a wierd shadowing");
  add_env namedef
  

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* catch all the decl to grow the environment *)

let rec (annotate_program2: program -> program) = fun prog ->

  (* globals (re)initialialisation *) 
  _scoped_env := initial_env;
  _notyped_var := (Hashtbl.create 100);


  let bigf = { Visitor_c.default_visitor_c_s with 

    Visitor_c.kexpr_s = (fun (k,bigf) e -> 
      let ((unwrap_e, oldtyp), iie) = e in
      match unwrap_e with
      (* don't want a warning on the Ident that are a FunCall *)
      | FunCall (((Ident f, typ), ii), args) -> 
          (FunCall (((Ident f, typ), ii), 
                   args +> List.map (fun (e,ii) -> 
                     Visitor_c.vk_argument_s bigf e, ii
                   )), oldtyp), iie
            
            
      | Ident (s) -> 
          (match (Common.optionise (fun () -> lookup_var s !_scoped_env)) with
          | Some (typ,_nextenv) -> Ast_c.rewrap_type_expr (Some typ) e
          | None  -> 
              if not (s =~ "[A-Z_]+") (* if macro then no warning *)
              then 
                if not (Hashtbl.mem !_notyped_var s)
                then begin 
                  pr2 ("Type_annoter: not finding type for " ^ s);
                  Hashtbl.add !_notyped_var s true;
                end;
              e 
          )

      | RecordAccess  (e, fld) ->  
          let e' = Visitor_c.vk_expr_s bigf e in
          let defaulte = (RecordAccess (e', fld), oldtyp), iie in
          (try 
            (match Ast_c.get_type_expr e' with 
            | None -> defaulte
            | Some t -> 
              (* TODO don't work, don't have good env *)
              let t' = find_final_type t !_scoped_env in
              (match Ast_c.unwrap_typeC t' with 
              | StructUnion (sopt, structtyp) -> 
                  let typefield = find_type_field fld structtyp in
                  ((RecordAccess (e', fld)), Some typefield), iie
              | _ -> defaulte
              )
            )
          with Not_found -> defaulte
          )

      | RecordPtAccess (e, fld) -> 
          let e' = Visitor_c.vk_expr_s bigf e in
          let defaulte = (RecordPtAccess (e', fld), oldtyp), iie in
          (try 
            (match Ast_c.get_type_expr e' with 
            | None -> defaulte
            | Some t -> 
              (* TODO don't work, don't have good env *)
              let t' = find_final_type t !_scoped_env in
              (match Ast_c.unwrap_typeC t' with 
              | Pointer (t) -> 
                  (match Ast_c.unwrap_typeC t with
                  | StructUnion (sopt, structtyp) -> 
                      let typefield = find_type_field fld structtyp in
                      ((RecordPtAccess (e', fld)), (Some typefield)), iie
                  | _ -> defaulte
                  )
              | _ -> defaulte
              )
            )
          with Not_found -> defaulte
          )
          

      | _ -> k e
    );
    Visitor_c.kstatement_s = (fun (k, bigf) st -> 
      match st with 
      | Compound statxs, ii -> do_in_new_scope (fun () -> k st);
      | _ -> k st

    );
    Visitor_c.kdecl_s = (fun (k, bigf) d -> 
      let d' = k d in
      let (DeclList (xs, ii)) = d in
      xs +> List.iter (fun ((var, t, sto), iicomma) -> 
        
        var +> do_option (fun ((s, ini), ii_s_ini) -> 
          match sto with 
          | StoTypedef, _inline -> 
              add_binding (TypeDef (s,Lib.al_type t)) true;
          | _ -> 
              add_binding (VarOrFunc (s, Lib.al_type t)) true;
        );
      );
      d'
        
    );

    Visitor_c.ktype_s = (fun (k, bigf) typ -> 
      let (q, t) = Lib.al_type typ in
      match Ast_c.unwrap t with 
      | StructUnion  ((Some s),  (su, structType)) -> 
          add_binding (StructUnionNameDef (s, (su, structType))) true;
          k typ (* todo: restrict ? new scope so use do_in_scope ? *)
      | _ -> k typ
          
    );    

    Visitor_c.kprogram_s = (fun (k, bigf) elem -> 
      _notyped_var := Hashtbl.create 100;
      match elem with
      | Definition def -> 
          let (funcs, ((returnt, (paramst, b)) as ftyp), sto, statxs), _ = def
          in
          let iitodo = [] in
          let typ' = Lib.al_type (Ast_c.nullQualif, (FunctionType ftyp, iitodo))
          in
          add_binding (VarOrFunc (funcs, typ')) false;
          do_in_new_scope (fun () -> 
            paramst +> List.iter (fun (((b, s, t), _),_) -> 
              match s with 
              | Some s -> add_binding (VarOrFunc (s,Lib.al_type t)) true
              | None -> pr2 "no type, certainly because Void type ?"
            );
            k elem
          );


      | _ -> k elem
    );
  } 
  in

  add_binding 
    (VarOrFunc ("NULL", Lib.al_type (Parse_c.type_of_string "void *"))) true;

  prog +> List.map (Visitor_c.vk_program_s bigf)
  

let annotate_program a = 
  Common.profile_code "annotate_type" (fun () -> annotate_program2 a)

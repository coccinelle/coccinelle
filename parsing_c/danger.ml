(* This puts danger annotations on tokens that are duplicated in the AST,
namely in multi declarations.  There is an assumption that such dangerous
regions are not nested.  Indeed, a declaration cannot be within another
declaration. *)

open Common

let danger_start x = x.Ast_c.danger := Ast_c.DangerStart
let danger_end x = x.Ast_c.danger := Ast_c.DangerEnd
let danger x = x.Ast_c.danger := Ast_c.Danger
let nodanger x = x.Ast_c.danger := Ast_c.NoDanger

let update_danger danger =
  let ii_function (k,bigf) ii = danger ii in
  { Visitor_c.default_visitor_c with
    Visitor_c.kinfo =ii_function }

let undanger = update_danger nodanger

let undanger_type = function
    (* undanger the right bits *)
    (tq,attr,(Ast_c.Array (sz,ty),ii)) ->
      List.iter (Visitor_c.vk_info undanger) ii;
      Common.do_option (Visitor_c.vk_expr undanger) sz
  | (tq,attr,(Ast_c.Pointer _,ii)) -> List.iter (Visitor_c.vk_info undanger) ii
  | (tq,attr,(Ast_c.FunctionType(ret,(params,(dots,dotsii))),ii)) ->
      List.iter (Visitor_c.vk_info undanger) ii;
      List.iter (Visitor_c.vk_info undanger) dotsii;
      Visitor_c.vk_param_list undanger params
  | _ -> ()

let undanger_onedecl (onedecl,_ii) =
  match onedecl.Ast_c.v_namei with
    Some (name,iniopt) ->
      Visitor_c.vk_name undanger name;
      (match iniopt with
        Ast_c.NoInit -> ()
      |	Ast_c.ValInit(init,iini) ->
	  List.iter nodanger iini;
	  Visitor_c.vk_ini undanger init);
      undanger_type onedecl.Ast_c.v_type;
  | None -> ()

let undanger_fieldkind (fieldkind,_ii) =
  match fieldkind with
    Ast_c.Simple(None,_,_) | Ast_c.BitField(None,_,_,_) ->
      (* no name implies nothing to do *)
      ()
  | Ast_c.Simple(Some name,ft,attrs) ->
      Visitor_c.vk_name undanger name;
      undanger_type ft;
      attrs +> List.iter (Visitor_c.vk_attribute undanger)
  | Ast_c.BitField(Some name,ft,ii,ce) ->
      Visitor_c.vk_name undanger name;
      undanger_type ft;
      Visitor_c.vk_info undanger ii;
      Visitor_c.vk_expr undanger ce

let add_danger xs =
  let decl_function (k,bigf) decl =
    match decl with
      Ast_c.DeclList (((_::_::_) as xs), ii) ->
	Visitor_c.vk_decl (update_danger danger) decl;
	xs +> List.iter undanger_onedecl;
	let (max,min) =
	  Lib_parsing_c.max_min_ii_by_pos(Lib_parsing_c.ii_of_decl decl) in
	(* put danger start on the first non-fake node; want all other nodes to be non dangerous for removal
	  Lib_parsing_c.max_min_ii_by_pos_filtered
	    (fun x -> not(Ast_c.is_fake x))
	    (Lib_parsing_c.ii_of_decl decl) in
	*)
	danger_start min;
	danger_end max
    | _ -> k decl in

  let struct_field_function (k,bigf) field =
    match field with
      Ast_c.DeclarationField
        (Ast_c.FieldDeclList (((_::_::_) as xs), iiptvirg)) ->
	Visitor_c.vk_struct_field (update_danger danger) field;
	xs +> List.iter undanger_fieldkind;
	let (max,min) =
	  Lib_parsing_c.max_min_ii_by_pos(Lib_parsing_c.ii_of_field field) in
	danger_start min;
	danger_end max
    | _ -> k field in

  let bigf = { Visitor_c.default_visitor_c with
	       Visitor_c.kdecl = decl_function;
	       Visitor_c.kfield = struct_field_function } in
  xs +> List.iter (fun p -> Visitor_c.vk_toplevel bigf p);

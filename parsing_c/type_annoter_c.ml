open Common open Commonop
(* 
 can either:
  - do a kind of inferer
     * can first do a simple inferer, that just pass context
     * then a real inferer, managing partial info.
  - extract the information from the .h files

 todo: expression contain types, and statements,   which in turn can contain
 expression, so need recurse. Need define an annote_statement and 
 annotate_type.

type environment = (string, fullType) Common.assoc

type context = fullType option

let boolType = Ast_c.nullQualif, Ast_c.defaultInt
let mktyp x = Ast_c.nullQualif, x
*)

open Ast_c


let rec (annotate_program: program -> program) = fun prog ->
  (* catch all the decl to grow the environment *)
  
  let env = ref (Hashtbl.create 100) in
  let scoped_env = ref [[]] in
  let notyped_var = ref (Hashtbl.create 100) in

  let new_scope() = scoped_env := []::!scoped_env in
  let del_scope() = 
    begin
      List.hd !scoped_env +> List.iter (fun s -> Hashtbl.remove !env s);
      scoped_env := List.tl !scoped_env
    end
  in
  let do_in_new_scope f = 
    begin
      new_scope();
      let res = f() in
      del_scope();
      res
    end
  in
  (* the warning argument is here to allow some binding to overwrite an 
   * existing one. With function, we first have the protype and then the def
   * and the def binding the same string is not an error.
   * todo?: but if we define two times the same function, then we will not
   * detect it :( would require to make a diff between adding a binding 
   * from a prototype and from a definition.
   *)
  let add_binding s typ warning = 
      let (current, older) = Common.uncons !scoped_env in

      if Hashtbl.mem !notyped_var s
      then pr2 ("warning: found typing information for a variable that was" ^
                "previously unknown:" ^ s);
              

      if List.mem s current && warning
      then begin 
        pr2 ("Type_annoter: warning, " ^ s ^ 
             " is already in current binding" ^ "\n" ^
             " so there is a wierd shadowing");
        Hashtbl.replace !env s typ;
        scoped_env := current::older; (* no change *)
      end
      else begin
        scoped_env := (s::current)::older;
        Hashtbl.add !env s typ;
      end
  in

  
  let bigf = { Visitor_c.default_visitor_c_s with 
    Visitor_c.kexpr_s = (fun (k,bigf) e -> 
      let ((unwrap_e, oldtyp), iie) = e in
      match unwrap_e with
      (* don't want a warning on the Ident that are a FunCall *)
      | FunCall (((Ident f, typ), ii), args) -> 
         (FunCall (((Ident f, typ), ii), 
                 args +> List.map (fun (e,ii) -> 
                   (match e with
                   | Left e -> Left (Visitor_c.visitor_expr_k_s bigf e)
                   | Right (t, stoil) -> 
                       let (unwrap_st, ii) = stoil in
                       Right (Visitor_c.visitor_type_k_s bigf t, 
                              (unwrap_st, 
                               List.map (Visitor_c.visitor_info_k_s bigf) ii
                                 ))
                   ), List.map (Visitor_c.visitor_info_k_s bigf) ii
                   )
                  ), oldtyp), iie
          
      | Ident (s) -> 
          (match (Common.optionise (fun () -> Hashtbl.find !env s)) with
          | Some typ -> (unwrap_e, Some typ), iie
          | None -> 
              if not (s =~ "[A-Z_]+") (* if macro then no warning *)
              then 
                if not (Hashtbl.mem !notyped_var s)
                then begin 
                  pr2 ("Type_annoter: not finding type for " ^ s);
                  Hashtbl.add !notyped_var s true;
                end;
              e 
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
           add_binding s t true
             );
         );
       d'
       
                         );
     Visitor_c.kprogram_s = (fun (k, bigf) elem -> 
       notyped_var := Hashtbl.create 100;
       match elem with
       | Definition def -> 
           let (funcs, ((returnt, (paramst, b)) as ftyp), sto, statxs), _ = def
           in
           let iitodo = [] in
           add_binding 
            funcs (Ast_c.nullQualif, (FunctionType ftyp, iitodo)) false;
           do_in_new_scope (fun () -> 
             paramst +> List.iter (fun (((b, s, t), _),_) -> 
               match s with 
               | Some s -> add_binding s t true
               | None -> pr2 "no type, certainly because Void type ?"
             );
             k elem
             );


       | _ -> k elem
             );
             } 
  in

  prog +> List.map (fun elem -> elem +> Visitor_c.visitor_program_k_s bigf)
  

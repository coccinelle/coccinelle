type cocci_predicate = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif
type formula =
    (cocci_predicate,Ast_cocci.meta_name, Wrapper_ctl.info) Ast_ctl.generic_ctl

let popl (name,_,_,ast) =
  match ast with
    [ast] ->
      let ast = Asttopopl.top ast in
      let ba = Insert_befaft.insert_befaft ast in
      let qt = Insert_quantifiers.insert_quantifiers ba in
      [Popltoctl.toctl qt]
  | _ -> failwith "only one rule allowed"

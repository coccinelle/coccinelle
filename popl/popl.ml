(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

type cocci_predicate = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif
type formula =
    (cocci_predicate,Ast_cocci.meta_name, Wrapper_ctl.info) Ast_ctl.generic_ctl

let poplz (name,_,ast) =
  match ast with
    [ast] ->
      let ast = Asttopopl.top ast in
      let ba = Insert_befaft.insert_befaft ast in
      let qt = Insert_quantifiers.insert_quantifiers ba in
      [Popltoctl.toctl qt]
  | _ -> failwith "only one rule allowed"

let popl r =
  match r with
    Ast_cocci.CocciRule (a,b,c) -> poplz (a,b,c)
  | _ -> []

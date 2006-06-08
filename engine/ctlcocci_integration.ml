open Common open Commonop

open Ograph_extended

(* -------------------------------------------------------------------- *)
(* Substitutions and instantiated CTL                                   *)
(* -------------------------------------------------------------------- *)

type mvar = string

type metavar_binding_kind2 = 
  | NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string

(* Substitutions map metavar's to *)
type substitution = (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution

type ctl_cocci = (Ast0toctl.predicate, mvar) Ast_ctl.generic_ctl

let (-->) x v = Ast_ctl.Subst (x,v);;

(* -------------------------------------------------------------------- *)
let (ctl_get_all_predicates: ctl_cocci -> Ast0toctl.predicate list) = fun ctl -> 
  let rec aux x =
    match x with
      Ast_ctl.False -> []
    | Ast_ctl.True -> []
    | Ast_ctl.Pred(p,_VTODO) -> [p]
    | Ast_ctl.Not(f) -> aux f
    | Ast_ctl.Exists(vars,f) -> aux f
    | Ast_ctl.And(f1,f2) ->     Common.union_set (aux f1) (aux f2)
    | Ast_ctl.Or(f1,f2) ->      Common.union_set (aux f1) (aux f2)
    | Ast_ctl.Implies(f1,f2) -> Common.union_set (aux f1) (aux f2)
    | Ast_ctl.AF(f) -> aux f
    | Ast_ctl.AX(f) -> aux f
    | Ast_ctl.AG(f) -> aux f
    | Ast_ctl.AU(f1,f2) -> Common.union_set (aux f1) (aux f2)
    | Ast_ctl.EF(f) -> aux f
    | Ast_ctl.EX(f) -> aux f
    | Ast_ctl.EG(f) -> aux f
    | Ast_ctl.EU(f1,f2) -> Common.union_set (aux f1) (aux f2) in
  aux ctl


(*
Take list of pred  and for each pred return where in control flow
it matches (and the set of subsitutions for this match).
*)

let top_wit = []

let (labels_for_ctl: ((nodei * Control_flow_c.node) list) -> (Ast0toctl.predicate list) -> 
   (Ast0toctl.predicate,  ((nodei * (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution * 'hole list) list)) assoc) 
  = fun nodes preds ->

   (* build assoc *)
   let assoc = preds +> List.map (fun pred -> 
       let nodes = nodes +> map (fun (nodei, (node, nodestring)) -> 
         (match pred, node with
         | Ast0toctl.Paren s,  (Control_flow_c.StartBrace (bracelevel, _)) -> 
             [(nodei,         [(s -->   (ParenVar (i_to_s bracelevel)))], top_wit)]
         | Ast0toctl.Paren s,  (Control_flow_c.EndBrace bracelevel) -> 
             [(nodei,         [(s -->   (ParenVar (i_to_s bracelevel)))], top_wit)]
         | Ast0toctl.Paren _, _ -> 
             []

         | Ast0toctl.Match (re),    node -> 
             let substs = Pattern.match_re_node re (node, nodestring)  (Ast_c.empty_metavars_binding) in
             if substs <> []
             then
               substs +> List.map (fun subst -> 
                 (nodei, 
                  subst +> List.map (fun (s, meta) -> 
                    s --> NormalMetaVar meta
                                    ),
                  top_wit
                 )
                )
             else []

         | Ast0toctl.TrueBranch , _ -> raise Todo
         | Ast0toctl.FalseBranch, _ -> raise Todo
         | Ast0toctl.After, _ -> raise Todo
         )
       ) +> List.concat
       in
       (pred, nodes)
       ) 
   in
   assoc

let (control_flow_for_ctl: (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> ('a, 'b) ograph_extended) = fun cflow ->
 (* could erase info on nodes, and edge,  because they are not used by rene *)
  cflow


(* Just make the final node of the control flow loop over itself. 
   It seems that one hypothesis of the SAT algorithm is that each node as at least a successor.
*)
let (fix_flow_ctl: (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> (Control_flow_c.node, Control_flow_c.edge) ograph_extended) = fun  flow ->
  let (exitnodei, (node, nodestr)) = flow#nodes#tolist +> List.find (function (nodei, (Control_flow_c.Exit, nodes)) -> true | _ -> false) in
  let flow = flow#add_arc ((exitnodei, exitnodei), Control_flow_c.Direct) in
  assert (flow#nodes#tolist +> List.for_all (fun (nodei, node) -> 
    List.length ((flow#successors nodei)#tolist) >= 1 
    (* no:  && List.length ((flow#predecessors nodei)#tolist) >= 1  
       because    the enter node at least have no predecessors 
     *)
      ));
  flow






let model_for_ctl  cflow ctl = 
 let newflow = fix_flow_ctl (control_flow_for_ctl cflow) in
 let labels = Common.assoc_to_function (labels_for_ctl (cflow#nodes#tolist) (ctl_get_all_predicates ctl)) in
 let states = List.map fst  newflow#nodes#tolist  in
 newflow, labels, states
 




type ('value, 'pred) wrapped_binding = 
  | ClassicVar of 'value
  | PredVar of 'pred Ast_ctl.modif


let (convert_label: 
  ('pred -> ('nodei * ('mvar, 'value) Ast_ctl.generic_substitution * 'a list) list)  -> 
   (('pred * 'mvar Ast_ctl.modif) -> 
     ('nodei * ('mvar, ('value, 'pred) wrapped_binding)   
        Ast_ctl.generic_substitution * 'a list) 
       list))  = fun oldlabelfunc ->  fun (p, predvar) -> 
         raise Common.Todo


module CTL_ENGINE_BIS =
  functor (SUB : Ctl_engine.SUBST) ->
    functor (G : Ctl_engine.GRAPH) ->
      struct
        let (satbis :
            G.cfg *
            ('pred -> (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_substitution * 'a list) list) *
            G.node list -> 
            ('pred * SUB.mvar Ast_ctl.modif, SUB.mvar) Ast_ctl.generic_ctl -> 
            (G.node * (SUB.mvar * SUB.value) list  * 'pred) list) = 
          fun x -> raise Common.Todo

      end
(* This module must convert the labelling function passed as parameter, by
   using convert_label. Then create a SUBST2 module handling the wrapped_binding.
   Then it can instantiates the generic CTL_ENGINE module. Call sat.
   And then process the witness tree to remove all that is not revelevant for
   the transformation phase.
*)

             
type ('pred, 'mvar) wrapped_ctl = ('pred * 'mvar Ast_ctl.modif,  'mvar) Ast_ctl.generic_ctl

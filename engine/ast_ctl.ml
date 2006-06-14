open Common open Commonop

open Ograph_extended



type mvar = string

type metavar_binding_kind2 = 
  | NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string

module ENV =
  struct
    type value = metavar_binding_kind2      
    type mvar = string
    let eq_mvar x x' = x = x';;
    let eq_val v v' = v = v';;
    let merge_val v v' = v;;		(* ok, since we are using '=' for eq. *)
  end


module CFG = 
  struct
    type node = int;;
    type cfg = (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended;;
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist);;
  end
;;

module ENGINE = Ctl_engine.CTL_ENGINE (ENV) (CFG)

include ENGINE


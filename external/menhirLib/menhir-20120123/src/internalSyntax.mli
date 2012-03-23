(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

type grammar = 
    {
      p_preludes	   : Stretch.t list;
      p_postludes          : Syntax.trailer list;
      p_parameters         : Stretch.t list;
      p_start_symbols      : Positions.t StringMap.t;
      p_types              : (Syntax.parameter * Stretch.ocamltype Positions.located) list;
      p_tokens	           : Syntax.token_properties StringMap.t;
      p_rules	           : Syntax.parameterized_rule StringMap.t;
    }

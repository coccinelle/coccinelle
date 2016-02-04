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

(* $Id: concreteSyntax.mli,v 1.3 2005/12/01 16:20:06 regisgia Exp $ *)
type grammar =
    { 
      pg_filename	   : Syntax.filename;
      pg_declarations	   : (Syntax.declaration Positions.located) list;
      pg_rules             : Syntax.parameterized_rule list;
      pg_trailer	   : Syntax.trailer option;
    }

    

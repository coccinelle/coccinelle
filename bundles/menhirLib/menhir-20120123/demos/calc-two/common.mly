/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  Fran�ois Pottier, INRIA Rocquencourt                                  */
/*  Yann R�gis-Gianas, PPS, Universit� Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

(* This partial grammar specification defines the grammar's entry
   point to be an expression, followed with an end-of-line token. *)

%start <int> main

%%

main:
| e = expr EOL
    { e }


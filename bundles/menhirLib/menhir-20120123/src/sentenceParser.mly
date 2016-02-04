/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is enabled. */

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. */

%{

 open Grammar
   
%}

%token COLON EOF EOL
%token<Grammar.Terminal.t> TERMINAL
%token<Grammar.Nonterminal.t> NONTERMINAL

%type <(Grammar.Nonterminal.t option * Grammar.Terminal.t list) option> sentence
%start sentence

%%

sentence:
| EOF
    { None } 
| NONTERMINAL COLON terminals EOL
    { Some (Some $1, $3) }
| terminals EOL
    { Some (None, $1) }

terminals:
| 
    { [] } 
| TERMINAL terminals
    { $1 :: $2 }


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

(* $Id: positions.ml,v 1.9 2005/12/01 16:20:07 regisgia Exp $ *)

(* TEMPORARY vérifier que ces fonctions sont utilisées partout et
             de façon cohérente; interaction avec [Error]? *)

open Lexing

type t = 
    { 
      start_p : Lexing.position; 
      end_p   : Lexing.position 
    }

type 'a located =
    {
      value    : 'a;
      position : t;
    }

let value { value = v } =
  v

let position { position = p } =
  p

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let with_poss p1 p2 v =
  with_pos { start_p = p1; end_p = p2 } v

let map f v =
  {
    value     = f v.value;
    position  = v.position;
  }

let iter f { value = v } =
  f v

let mapd f v =
  let w1, w2 = f v.value in
  let pos = v.position in
  { value = w1; position = pos },
  { value = w2; position = pos }

let dummy = 
  {
    start_p = Lexing.dummy_pos;
    end_p   = Lexing.dummy_pos
  }

let unknown_pos v = 
  {
    value     = v;
    position  = dummy
  }

let start_of_position p = p.start_p

let end_of_position p = p.end_p

let filename_of_position p = 
  p.start_p.Lexing.pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let join x1 x2 =
{
  start_p = if x1 = dummy then x2.start_p else x1.start_p;
  end_p   = if x2 = dummy then x1.end_p else x2.end_p
}

let lex_join x1 x2 =
{
  start_p = x1;
  end_p   = x2
}

let join_located l1 l2 f = 
  {
    value    = f l1.value l2.value;
    position = join l1.position l2.position;
  }

let string_of_lex_pos p = 
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let string_of_pos p = 
  let filename = filename_of_position p in
  assert (filename <> "");
  let l = line p.start_p in
  let c1, c2 = characters p.start_p p.end_p in
  Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let pos_or_undef = function
  | None -> dummy
  | Some x -> x

let cpos lexbuf =
  { 
    start_p = Lexing.lexeme_start_p lexbuf;
    end_p   = Lexing.lexeme_end_p   lexbuf; 
  }

let with_cpos lexbuf v =
  with_pos (cpos lexbuf) v

let string_of_cpos lexbuf = 
  string_of_pos (cpos lexbuf)

let joinf f t1 t2 = 
  join (f t1) (f t2)

let ljoinf f =
  List.fold_left (fun p t -> join p (f t)) dummy

let join_located_list ls f = 
  {
    value     = f (List.map (fun l -> l.value) ls);
    position  = ljoinf (fun x -> x.position) ls
  }

(* The functions that print error messages and warnings require a list of
   positions. The following auxiliary functions help build such lists. *)

type positions =
    t list

let one (pos : Lexing.position) : positions =
  [ { start_p = pos; end_p = pos } ] (* or: lex_join pos pos *)

let two (pos1 : Lexing.position) (pos2 : Lexing.position) : positions =
  [ lex_join pos1 pos2 ]

let lexbuf (lexbuf : Lexing.lexbuf) : positions =
  [ lex_join lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p ]


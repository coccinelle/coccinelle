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

(* Let's do floating-point evaluation, for a change. *)

module FloatSemantics = struct

  type number =
      float

  let inject =
    float_of_int

  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let (~- ) = (~-. )

end

(* Let us now specialize our parameterized parser. *)

module FloatParser =
  Parser.Make(FloatSemantics)

(* The rest is as usual. *)

let () =
  let stdinbuf = Lexing.from_channel stdin in
  while true do
    (* Read line by line. *)
    let linebuf = Lexing.from_string (Lexer.line stdinbuf) in
    try
      (* Run the parser on a single line of input. *)
      Printf.printf "%.1f\n%!" (FloatParser.main Lexer.token linebuf)
    with
    | Lexer.Error msg ->
	Printf.fprintf stderr "%s%!" msg
    | FloatParser.Error ->
	Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
  done

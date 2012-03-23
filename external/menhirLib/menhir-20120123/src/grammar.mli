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

(* This module transforms [Front.grammar], an abstract syntax tree for
   the grammar, into an internal representation of the grammar that is
   more usable. *)

(* ------------------------------------------------------------------------ *)
(* Nonterminals. *)

module Nonterminal : sig

  (* The type of nonterminals. *)

  type t

  (* The number of nonterminals. This includes the extra nonterminals
     that are internally generated for the grammar's entry points. *)

  val n: int

  (* [lookup] maps an identifier to a nonterminal, or raises [Not_found]. *)

  val lookup : string -> t

  (* Nonterminals can be converted to integers. This feature is
     exploited in the table-based back-end. *)

  val n2i: t -> int

  (* This produces a string representation of a nonterminal. It should
     in principle never be applied to one of the internally generated
     nonterminals, as we do not wish users to become aware of the
     existence of these extra nonterminals. However, we do sometimes
     violate this rule when it is difficult to do otherwise.

     The Boolean parameter tells whether the string representation
     should be normalized, that is, whether parentheses and commas
     should be eliminated. This is necessary if the string is intended
     for use as a valid nonterminal name or as a valid Objective Caml
     identifier. *)

  val print: bool -> t -> string

  (* This is the Objective Caml type associated with a nonterminal
     symbol. It is known only if a %type declaration was provided.
     This function is not applicable to the internally generated
     nonterminals. *)

  val ocamltype: t -> Stretch.ocamltype option

  (* Iteration over nonterminals. The order in which elements are
     examined, and the order of [map]'s output list, correspond to the
     numeric indices produced by [n2i] above. *)

  val iter: (t -> unit) -> unit
  val fold: (t -> 'a -> 'a) -> 'a -> 'a
  val map: (t -> 'a) -> 'a list

  (* Iteration over all nonterminals, except the start nonterminals. *)

  val iterx: (t -> unit) -> unit
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a 

  (* Tabulation of a function over nonterminals. *)

  val tabulate: (t -> 'a) -> (t -> 'a)

  (* [positions nt] is a list of the positions associated with the
     definition of [nt]. There can be more than one position because
     definitions can be split over multiple files. *)

  val positions: t -> Positions.t list

  (* This tells whether a non-terminal symbol is one of the start
     symbols. *)

  val is_start: t -> bool

end

(* ------------------------------------------------------------------------ *)
(* Terminals. *)

module Terminal : sig

  (* The type of terminals. *)

  type t

  (* The number of terminals. This includes the two pseudo-tokens
     [#] and [error]. *)

  val n: int

  (* Comparison. *)

  val equal: t -> t -> bool

  (* [lookup] maps an identifier to a terminal, or raises [Not_found]. *)

  val lookup : string -> t

  (* Terminals can be converted to integers. This feature is exploited
     in the table-based back-end. *)

  val t2i: t -> int

  (* This produces a string representation of a terminal. *)

  val print: t -> string

  (* This is the Objective Caml type associated with a terminal
     symbol. It is known only if the %token declaration was
     accompanied with a type. *)

  val ocamltype: t -> Stretch.ocamltype option

  (* These are the two pseudo-tokens [#] and [error]. The former is
     used to denote the end of the token stream. The latter is
     accessible to the user and is used for handling errors. *)

  val sharp: t
  val error: t

  (* This is the programmer-defined [EOF] token, if there is one. It
     is recognized based solely on its name, which is fragile, but
     this behavior is documented. This token is assumed to represent
     [ocamllex]'s [eof] pattern. It is used only in emitting warnings
     in [--error-recovery] mode. *)

  val eof: t option

  (* This returns [true] if and only if the token at hand is one of
     [#] or [error]. *)

  val pseudo: t -> bool

  (* Iteration over terminals. The order in which elements are
     examined, and the order of [map]'s output list, correspond to the
     numeric indices produced by [t2i] above. [mapx] offers iteration
     over all terminals except [#]. *)

  val iter: (t -> unit) -> unit
  val fold: (t -> 'a -> 'a) -> 'a -> 'a
  val map: (t -> 'a) -> 'a list
  val mapx: (t -> 'a) -> 'a list

end

(* ------------------------------------------------------------------------ *)
(* Sets and maps over terminals. *)

module TerminalSet : sig

  (* All of the operations documented in [GSet] are available. *)

  include GSet.S with type element = Terminal.t

  (* This offers a string representation of a set of terminals. The
     symbols are simply listed one after the other and separated with
     spaces. *)

  val print: t -> string

  (* This is the set of all terminal symbols except the pseudo-tokens
     [#] and [error]. *)

  val universe: t

end

(* All of the operations documented in [GMap] are available. *)

module TerminalMap : GMap.S with type key = Terminal.t

(* ------------------------------------------------------------------------ *)
(* Symbols. *)

module Symbol : sig

  (* A symbol is either a nonterminal or a terminal. *)

  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  (* Comparison. *)

  val equal: t -> t -> bool
  val lequal: t list -> t list -> bool

  (* These produce a string representation of a symbol, of a list of
     symbols, or of an array of symbols. The symbols are simply listed
     one after the other and separated with spaces. [printao] prints
     an array of symbols, starting at a particular offset. [printaod]
     is analogous, but can also print a single dot at a particular
     position between two symbols. *)

  val print: t -> string
  val printl: t list -> string
  val printa: t array -> string
  val printao: int -> t array -> string
  val printaod: int -> int -> t array -> string

end

(* ------------------------------------------------------------------------ *)
(* Sets and maps over symbols. *)

(* All of the operations documented in [Set] are available. *)

module SymbolSet : Set.S with type elt = Symbol.t

module SymbolMap : sig

  (* All of the operations documented in [Map] are available. *)

  include Map.S with type key = Symbol.t

  val domain: 'a t -> key list

  (* This returns [true] if and only if all of the symbols in
     the domain of the map at hand are nonterminals. *)

  val purelynonterminal: 'a t -> bool

end

(* ------------------------------------------------------------------------ *)
(* Productions. *)

module Production : sig

  (* This is the type of productions. This includes user-defined
     productions as well as the internally generated productions
     associated with the start symbols. *)

  type index

  (* Productions can be converted to integers and back. This is unsafe
     and should be avoided as much as possible. This feature is
     exploited, for efficiency, in the encoding of items. *)

  val p2i: index -> int
  val i2p: int -> index

  (* The number of productions. *)

  val n: int

  (* These map a production index to the production's definition, that
     is, a nonterminal (the left-hand side) and an array of symbols
     (the right-hand side). *)

  val def: index -> Nonterminal.t * Symbol.t array
  val nt: index -> Nonterminal.t
  val rhs: index -> Symbol.t array
  val length: index -> int

  (* This maps a production index to an array of the identifiers that
     should be used for naming the semantic values of the symbols in
     the right-hand side. *)

  val identifiers: index -> Syntax.identifier array

  (* This maps a production index to an array of Boolean flag. Each
     flag tells whether the semantic value of the corresponding symbol
     is used in the semantic action. This is a conservative
     approximation: [true] means maybe, while [false] means certainly
     not. *)

  val used: index -> bool array  

  (* This maps a production index to the production's semantic action.
     This function is not applicable to a start production. *)

  val action: index -> Syntax.action

  (* [positions prod] is a list of the positions associated with
     production [prod]. This is usually a singleton list, but there
     can be more than one position for start productions when the
     definition of the corresponding start symbol is split over
     multiple files. *)

  val positions: index -> Positions.t list

  (* Iteration over all productions. The order in which elements
     are examined, and the order of [map]'s output list, correspond
     to the numeric indices produced by [p2i] above. *)

  val iter: (index -> unit) -> unit
  val fold: (index -> 'a -> 'a) -> 'a -> 'a
  val map: (index -> 'a) -> 'a list

  (* Iteration over all productions, except the start productions. *)

  val iterx: (index -> unit) -> unit
  val foldx: (index -> 'a -> 'a) -> 'a -> 'a

  (* This maps a (user) non-terminal start symbol to the corresponding
     start production. *)

  val startsymbol2startprod: Nonterminal.t -> index

  (* Iteration over the productions associated with a specific
     nonterminal. *)

  val iternt: Nonterminal.t -> (index -> unit) -> unit
  val foldnt: Nonterminal.t -> 'a -> (index -> 'a -> 'a) -> 'a

  (* This allows determining whether a production is a start
     production. If it is a start production, the start symbol that it
     is associated with is returned. If it is a regular production,
     nothing is returned. *)

  val classify: index -> Nonterminal.t option

  (* This produces a string representation of a production. It should
     never be applied to a start production, as we do not wish users
     to become aware of the existence of these extra productions. *)

  val print: index -> string

  (* Tabulation of a Boolean function over nonterminals. [tabulate f]
     returns a tabulated version of [f] as well as the number of
     productions where [f] is true. *)

  val tabulate: (index -> bool) -> (index -> bool) * int

end

(* ------------------------------------------------------------------------ *)
(* Maps over productions. *)

module ProductionMap : sig

  include GMap.S with type key = Production.index

  (* Iteration over the start productions only. *)

  val start: (Production.index -> 'a) -> 'a t

end

(* ------------------------------------------------------------------------ *)
(* Analysis of the grammar. *)

module Analysis : sig

  (* [nullable_first_rhs rhs i] considers the string of symbols found at
     offset [i] in the array [rhs]. It returns its NULLABLE flag as well
     as its FIRST set. The offset [i] must be contained between [0] and
     [n], where [n] is the length of [rhs], inclusive. *)

  val nullable_first_rhs: Symbol.t array -> int -> bool * TerminalSet.t

  (* [explain_first_rhs tok rhs i] explains why the token [tok] appears
     in the FIRST set for the string of symbols found at offset [i] in
     the array [rhs]. *)

  val explain_first_rhs: Terminal.t -> Symbol.t array -> int -> string

  (* [follow nt] is the FOLLOW set of the non-terminal symbol [nt], that
     is, the set of terminal symbols that could follow an expansion of
     [nt] in a valid sentence. *)

  val follow: Nonterminal.t -> TerminalSet.t

end

(* ------------------------------------------------------------------------ *)
(* Conflict resolution via precedences. *)

module Precedence : sig

  (* Shift/reduce conflicts require making a choice between shifting a
     token and reducing a production. How these choices are made is of
     no concern to the back-end, but here is a rough explanation.

     Shifting is preferred when the token has higher precedence than
     the production, or they have same precedence and the token is
     right-associative.

     Reducing is preferred when the token has lower precedence than
     the production, or they have same precedence and the token is
     left-associative.

     Neither is allowed when the token and the production have same
     precedence and the token is non-associative.

     No preference is explicitly specified when the token or the
     production has undefined precedence. In that case, the default
     choice is to prefer shifting, but a conflict will be reported. *)

  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  val shift_reduce: Terminal.t -> Production.index -> choice

  (* Reduce/reduce conflicts require making a choice between reducing
     two distinct productions. This is done by exploiting a partial
     order on productions.

     For compatibility with ocamlyacc, this order should be total and
     should correspond to textual order when the two productions
     originate in the same source file. When they originate in
     different source files, the two productions should be
     incomparable. *)

  val reduce_reduce: Production.index -> Production.index -> Production.index option

end

(* ------------------------------------------------------------------------ *)
(* Diagnostics. *)

(* This function prints diagnostics about precedence declarations that
   are never consulted. It is called after the automaton is
   constructed. *)

val diagnostics: unit -> unit


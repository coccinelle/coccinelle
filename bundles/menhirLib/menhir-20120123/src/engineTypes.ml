(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(**************************************************************************)

(* This file defines several types and module types that are used in the
   specification of module [Engine]. *)

(* --------------------------------------------------------------------------- *)

(* It would be nice if we could keep the structure of stacks and environments
   hidden. However, stacks and environments must be accessible to semantic
   actions, so the following data structure definitions must be public. *)

(* --------------------------------------------------------------------------- *)

(* A stack is a linked list of cells. A sentinel cell -- which is its own
   successor -- is used to mark the bottom of the stack. The sentinel cell
   itself is not significant -- it contains dummy values. *)

type ('state, 'semantic_value) stack = {

  (* The state that we should go back to if we pop this stack cell. *)

  (* This convention means that the state contained in the top stack cell is
     not the current state [env.current]. It also means that the state found
     within the sentinel is a dummy -- it is never consulted. This convention
     is the same as that adopted by the code-based back-end. *)

  state: 'state;

  (* The semantic value associated with the chunk of input that this cell
     represents. *)

  semv: 'semantic_value;

  (* The start and end positions of the chunk of input that this cell
     represents. *)

  startp: Lexing.position;
  endp: Lexing.position;

  (* The next cell down in the stack. If this is a self-pointer, then this
     cell is the sentinel, and the stack is conceptually empty. *)

  next: ('state, 'semantic_value) stack;

}

(* --------------------------------------------------------------------------- *)

(* A parsing environment contains basically all of the automaton's state. *)

type ('state, 'semantic_value, 'token) env = {

  (* The lexer. *)

  lexer: Lexing.lexbuf -> 'token;

  (* The lexing buffer. It is used as an argument to the lexer, and also
     accessed directly when extracting positions. *)

  lexbuf: Lexing.lexbuf;

  (* The last token that was obtained from the lexer. *)

  mutable token: 'token;

  (* A count of how many tokens were shifted since the beginning, or since
     the last [error] token was encountered. By convention, if [shifted]
     is (-1), then the current lookahead token is [error]. *)

  mutable shifted: int;

  (* A copy of the value of [shifted] just before the most recent error
     was detected. This value is not used by the automaton itself, but
     is made accessible to semantic actions. *)

  mutable previouserror: int;

  (* The stack. In [CodeBackend], it is passed around on its own,
     whereas, here, it is accessed via the environment. *)

  mutable stack: ('state, 'semantic_value) stack;

  (* The current state. In [CodeBackend], it is passed around on its
     own, whereas, here, it is accessed via the environment. *)

  mutable current: 'state;

}

(* --------------------------------------------------------------------------- *)

(* This signature describes the parameters that must be supplied to the LR
   engine. *)

module type TABLE = sig

  (* The type of automaton states. *)

  type state

  (* The type of tokens. These can be thought of as real tokens, that is,
     tokens returned by the lexer. They carry a semantic value. This type
     does not include the [error] pseudo-token. *)

  type token

  (* The type of terminal symbols. These can be thought of as integer codes.
     They do not carry a semantic value. This type does include the [error]
     pseudo-token. *)

  type terminal

  (* The type of semantic values. *)

  type semantic_value

  (* A token is conceptually a pair of a (non-[error]) terminal symbol and
     a semantic value. The following two functions are the pair projections. *)

  val token2terminal: token -> terminal
  val token2value: token -> semantic_value

  (* Even though the [error] pseudo-token is not a real token, it is a
     terminal symbol. Furthermore, for regularity, it must have a semantic
     value. *)

  val error_terminal: terminal
  val error_value: semantic_value

  (* The type of productions. *)

  type production

  (* If a state [s] has a default reduction on production [prod], then, upon
     entering [s], the automaton should reduce [prod] without consulting the
     lookahead token. The following function allows determining which states
     have default reductions. *)

  (* Instead of returning a value of a sum type -- either [DefRed prod], or
     [NoDefRed] -- it accepts two continuations, and invokes just one of
     them. This mechanism allows avoiding a memory allocation. *)

  val default_reduction:
    state ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (* An LR automaton can normally take three kinds of actions: shift, reduce,
     or fail. (Acceptance is a particular case of reduction: it consists in
     reducing a start production.) *)

  (* There are two variants of the shift action. [shift/discard s] instructs
     the automaton to discard the current token, request a new one from the
     lexer, and move to state [s]. [shift/nodiscard s] instructs it to move to
     state [s] without requesting a new token. This instruction should be used
     when [s] has a default reduction on [#]. See [CodeBackend.gettoken] for
     details. *)

  (* This is the automaton's action table. It maps a pair of a state and a
     terminal symbol to an action. *)

  (* Instead of returning a value of a sum type -- one of shift/discard,
     shift/nodiscard, reduce, or fail -- this function accepts three
     continuations, and invokes just one them. This mechanism allows avoiding
     a memory allocation. *)

  (* In summary, the parameters to [action] are as follows:

     - the first two parameters, a state and a terminal symbol, are used to
       look up the action table;

     - the next parameter is the semantic value associated with the above
       terminal symbol; it is not used, only passed along to the shift
       continuation, as explained below;

     - the shift continuation expects an environment; a flag that tells
       whether to discard the current token; the terminal symbol that
       is being shifted; its semantic value; and the target state of
       the transition;

     - the reduce continuation expects an environment and a production;

     - the fail continuation expects an environment;

     - the last parameter is the environment; it is not used, only passed
       along to the selected continuation. *)

  val action:
    state ->
    terminal ->
    semantic_value ->
    ('env -> bool -> terminal -> semantic_value -> state -> 'answer) ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (* This is the automaton's goto table. It maps a pair of a state and a
     production to a new state.

     This convention is slightly different from the textbook approach. The
     goto table is usually indexed by a state and a non-terminal symbol. *)

  val goto: state -> production -> state

  (* By convention, a semantic action is responsible for:

     1. fetching whatever semantic values and positions it needs off the stack;

     2. popping an appropriate number of cells off the stack, as dictated
     by the length of the right-hand side of the production; this involves
     updating [env.stack];

     3. computing a new semantic value, as well as new start and end positions;

     4. pushing a new stack cell, which contains the three values
     computed in step 3; this again involves updating [env.stack]
     (only one update is necessary).

     Point 1 is essentially forced upon us: if semantic values were fetched
     off the stack by this interpreter, then the calling convention for
     semantic actions would be variadic: not all semantic actions would have
     the same number of arguments. The rest follows rather naturally. *)

  (* If production [prod] is an accepting production, then the semantic action
     is responsible for raising exception [Accept], instead of returning
     normally. This convention allows us to not distinguish between regular
     productions and accepting productions. All we have to do is catch that
     exception at top level. *)

  (* Semantic actions are allowed to raise [Error]. *)

  exception Accept of semantic_value
  exception Error

  type semantic_action =
      (state, semantic_value, token) env -> unit

  val semantic_action: production -> semantic_action

  (* The LR engine can attempt error recovery. This consists in discarding
     tokens, just after an error has been successfully handled, until a token
     that can be successfully handled is found. This mechanism is optional.
     The following flag enables it. *)

  val recovery: bool

  (* The LR engine requires a number of hooks, which are used for logging. *)

  (* The comments below indicate the conventional messages that correspond
     to these hooks in the code-based back-end; see [CodeBackend]. *)

  module Log : sig

    (* State %d: *)

    val state: state -> unit

    (* Shifting (<terminal>) to state <state> *)

    val shift: terminal -> state -> unit

    (* Reducing a production should be logged either as a reduction
       event (for regular productions) or as an acceptance event (for
       start productions). *)

    (* Reducing production <production> / Accepting *)

    val reduce_or_accept: production -> unit

    (* Lookahead token is now <terminal> (<pos>-<pos>) *)

    val lookahead_token: Lexing.lexbuf -> terminal -> unit

    (* Initiating error handling *)

    val initiating_error_handling: unit -> unit

    (* Resuming error handling *)

    val resuming_error_handling: unit -> unit

    (* Handling error in state <state> *)

    val handling_error: state -> unit

    (* Discarding last token read (<terminal>) *)

    val discarding_last_token: terminal -> unit

  end

end

(* --------------------------------------------------------------------------- *)

(* This signature describes the LR engine. *)

module type ENGINE = sig

  type state

  type token

  type semantic_value

  (* An entry point to the engine requires a start state, a lexer, and a lexing
     buffer. It either succeeds and produces a semantic value, or fails and
     raises [Error]. *)

  exception Error

  val entry:
    state ->
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    semantic_value

end

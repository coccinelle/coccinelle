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

open CodeBits
open Grammar
open IL
open Interface
open PreFront
open Printf
open TokenType
open CodePieces

module Run (T : sig end) = struct

(* ------------------------------------------------------------------------ *)

(* Conventional names for modules, exceptions, record fields, functions. *)

let menhirlib =
  "MenhirLib"

let tableInterpreter =
  menhirlib ^ ".TableInterpreter"

let make =
  tableInterpreter ^ ".Make"

let accept =
  tableInterpreter ^ ".Accept"

let engineTypes =
  menhirlib ^ ".EngineTypes"

let field x =
  engineTypes ^ "." ^ x

let fstate =
  field "state"

let fsemv =
  field "semv"

let fstartp =
  field "startp"

let fendp =
  field "endp"

let fnext =
  field "next"

let fstack =
  field "stack"

let fcurrent =
  field "current"

let flexbuf =
  field "lexbuf"

let fpreviouserror =
  field "previouserror"

let flex_start_p =
  "Lexing.lex_start_p"

let interpreter =
  "MenhirInterpreter"

let entry =
  interpreter ^ ".entry"

(* ------------------------------------------------------------------------ *)

(* Code generation for semantic actions. *)

(* The functions [reducecellparams] and [reducebody] are adpated from
   [CodeBackend]. *)

(* Things are slightly more regular here than in the code-based
   back-end, since there is no optimization: every stack cell has the
   same structure and holds a state, a semantic value, and a pair of
   positions. Because every semantic value is represented, we do not
   have a separate [unitbindings]. *)

(* [reducecellparams] constructs a pattern that describes the contents
   of a stack cell. If this is the bottom cell, the variable [state]
   is bound to the state found in the cell. If [ids.(i)] is used in
   the semantic action, then it is bound to the semantic value. The
   position variables are always bound. *)

let reducecellparams prod i symbol (next : pattern) : pattern =

  let ids = Production.identifiers prod
  and used = Production.used prod in

  PRecord [
    fstate, (if i = 0 then PVar state else PWildcard);
    fsemv, (if used.(i) then PVar ids.(i) else PWildcard);
    fstartp, PVar (Printf.sprintf "_startpos_%s_" ids.(i));
    fendp, PVar (Printf.sprintf "_endpos_%s_" ids.(i));
    fnext, next;
  ]

(* The semantic values bound in [reducecellparams] have type [Obj.t].
   They should now be cast to their real type. If we had [PMagic] in
   the syntax of patterns, we could do that in one swoop; since we don't,
   we have to issue a series of casts a posteriori. *)

let reducecellcasts prod i symbol casts =

  let ids = Production.identifiers prod
  and used = Production.used prod in

  if used.(i) then
    let id = ids.(i) in
    let t : typ =
      match semvtype symbol with
      | [] ->
	  tunit
      | [ t ] ->
	  t
      | _ ->
	  assert false
    in
    (* Cast: [let id = ((Obj.magic id) : t) in ...]. *)
    (
      PVar id,
      EAnnot (EMagic (EVar id), type2scheme t)
    ) :: casts
  else
    casts

(* This is the body of the [reduce] function associated with
   production [prod]. It assumes that the variables [env] and [stack]
   have been bound. *)

let reducebody prod =

  let nt, rhs = Production.def prod
  and ids = Production.identifiers prod
  and length = Production.length prod in

  (* Build a pattern that represents the shape of the stack. Out of
     the stack, we extract a state (except when the production is an
     epsilon production) and a number of semantic values. *)

  (* At the same time, build a series of casts. *)

  let (_ : int), pat, casts =
    Invariant.fold (fun (i, pat, casts) (_ : bool) symbol _ ->
      i + 1,
      reducecellparams prod i symbol pat,
      reducecellcasts prod i symbol casts
    ) (0, PVar stack, []) (Invariant.prodstack prod)
  in

  (* Determine start and end positions for the left-hand side of the
     production. *)

  let posbindings =
    ( PVar startp,
      if length > 0 then
	EVar (Printf.sprintf "_startpos_%s_" ids.(0))
      else
	ERecordAccess(ERecordAccess (EVar env, flexbuf), flex_start_p)
    ) ::
    ( PVar endp,
      if length > 0 then
	EVar (Printf.sprintf "_endpos_%s_" ids.(length - 1))
      else
	EVar startp
    ) :: []
  in

  (* Is this is one of the start productions? *)

  match Production.classify prod with
  | Some nt ->

      (* This is a start production. Raise [Accept]. *)

      EComment (
        sprintf "Accepting %s" (Nonterminal.print false nt),
        blet (
  	  [ pat, EVar stack ],
	  ERaise (EData (accept, [ EVar ids.(0) ]))
	)
      )

  | None ->

      (* This is a regular production. Perform a reduction. *)

      let action =
	Production.action prod
      in
      let act =
	EAnnot (Action.to_il_expr action, type2scheme (semvtypent nt))
      in

      EComment (
        Production.print prod,
        blet (
	  (pat, EVar stack) ::                  (* destructure the stack *)
	  casts @                               (* perform type casts *)
	  posbindings @                         (* bind [startp] and [endp] *)
	  extrabindings fpreviouserror action @ (* add bindings for the weird keywords *)
	  [ PVar semv, act ],                   (* run the user's code and bind [semv] *)

	  ERecordWrite (
	    EVar env, fstack,                   (* update the stack with ... *)
	    ERecord [                           (* ... a new stack cell *)
	      fstate, EVar state;               (* the current state after popping; it will be updated by [goto] *)
	      fsemv, ERepr (EVar semv);         (* the newly computed semantic value *)
	      fstartp, EVar startp;             (* the newly computed start and end positions *)
	      fendp, EVar endp;
	      fnext, EVar stack;                (* this is the stack after popping *)
	    ]
	  )

	)
      )

let semantic_action prod =
  EFun (
    [ PVar env ],

    if Invariant.ever_reduced prod then

      (* Access the stack and current state via the environment. *)

      (* In fact, the current state needs be bound here only if this is
	 an epsilon production. Otherwise, the variable [state] will be
	 bound by the pattern produced by [reducecellparams] above. *)

      ELet (

	[ PVar stack, ERecordAccess (EVar env, fstack) ] @
	  (if Production.length prod = 0 then [ PVar state, ERecordAccess (EVar env, fcurrent) ] else []),

	(* Then, *)

	reducebody prod

      )

    else

      (* For productions that are never reduced, generate no code. *)

      (* We do this mainly because [Invariant.prodstack] does not
	 support productions that are never reduced. *)
      
      EComment (
	"a production never reduced",
	EApp (EVar "assert", [ EData ("false", []) ])
      )

  )

(* ------------------------------------------------------------------------ *)

(* Table encodings. *)

(* Encodings of entries in the default reduction table. *)

let encode_DefRed prod =            (* 1 + prod *)
  1 + Production.p2i prod

let encode_NoDefRed =               (* 0 *)
  0

(* Encodings of entries in the action table. *)

let encode_Reduce prod =            (* prod | 01 *)
  (Production.p2i prod lsl 2) lor 1

let encode_ShiftDiscard s =         (*    s | 10 *)
  ((Lr1.number s) lsl 2) lor 0b10

let encode_ShiftNoDiscard s =       (*    s | 11 *)
  ((Lr1.number s) lsl 2) lor 0b11

let encode_Fail =                   (*        00 *)
  0

(* Encodings of entries in the goto table. *)

let encode_Goto node =              (* 1 + node *)
  1 + Lr1.number node

let encode_NoGoto =                 (* 0 *)
  0

(* Encodings of the hole in the action and goto tables. *)

let hole =
  assert (encode_Fail = 0);
  assert (encode_NoGoto = 0);
  0

(* Encodings of entries in the error bitmap. *)

let encode_Error =                  (* 0 *)
  0

let encode_NoError =                (* 1 *)
  1

(* ------------------------------------------------------------------------ *)

(* Statistics. *)

(* Integer division, rounded up. *)

let div a b =
  if a mod b = 0 then a / b else a / b + 1

(* [size] provides a rough measure of the size of its argument, in words.
   The [unboxed] parameter is true if we have already counted 1 for the
   pointer to the object. *)

let rec size unboxed = function
  | EIntConst _
  | ETuple []
  | EData (_, []) ->
      if unboxed then 0 else 1
  | EStringConst s ->
      1 + div (String.length s * 8) Sys.word_size
  | ETuple es
  | EData (_, es)
  | EArray es ->
      1 + List.length es + List.fold_left (fun s e -> s + size true e) 0 es
  | _ ->
      assert false (* not implemented *)

let size =
  size false

(* Optionally, print a measure of each of the tables that we are defining. *)

let define (name, expr) = {
  valpublic = true;
  valpat = PVar name;
  valval = expr
}

let define_and_measure (x, e) =
  Error.logC 1 (fun f ->
    fprintf f
      "The %s table occupies roughly %d bytes.\n"
      x
      (size e * (Sys.word_size / 8))
  );
  define (x, e)


(* ------------------------------------------------------------------------ *)

(* Table compression. *)

(* Our sparse, two-dimensional tables are turned into one-dimensional tables
   via [RowDisplacement]. *)

(* The error bitmap, which is two-dimensional but not sparse, is made
   one-dimensional by simple flattening. *)

(* Every one-dimensional table is then packed via [PackedIntArray]. *)

(* Optionally, we print some information about the compression ratio. *)

(* [population] counts the number of significant entries in a
   two-dimensional matrix. *)

let population (matrix : int array array) =
  Array.fold_left (fun population row ->
    Array.fold_left (fun population entry ->
      if entry = hole then population else population + 1
    ) population row
  ) 0 matrix

(* [marshal1] marshals a one-dimensional array. *)

let marshal1 (table : int array) =
  let (bits : int), (text : string) = MenhirLib.PackedIntArray.pack table in
  ETuple [ EIntConst bits; EStringConst text ]

(* [marshal11] marshals a one-dimensional array whose bit width is
   statically known to be [1]. *)

let marshal11 (table : int array) =
  let (bits : int), (text : string) = MenhirLib.PackedIntArray.pack table in
  assert (bits = 1);
  EStringConst text

(* [marshal2] marshals a two-dimensional table. *)

let marshal2 name m n (matrix : int list list) =
  let matrix : int array array =
    Array.of_list (List.map Array.of_list matrix)
  in
  let (displacement : int array), (data : int array) =
    MenhirLib.RowDisplacement.compress
      (=)
      (fun x -> x = hole)
      hole
      m
      n
      matrix
  in
  Error.logC 1 (fun f ->
    fprintf f
      "The %s table is %d entries; %d non-zero; %d compressed.\n"
      name
      (m * n)
      (population matrix)
      (Array.length displacement + Array.length data)
  );
  ETuple [
    marshal1 displacement;
    marshal1 data;
  ]

let marshal1 (table : int list) =
  marshal1 (Array.of_list table)

let marshal11 (table : int list) =
  marshal11 (Array.of_list table)

(* ------------------------------------------------------------------------ *)

(* Table generation. *)

(* The action table. *)

let action node t =
  match Invariant.has_default_reduction node with
  | Some _ ->

      (* [node] has a default reduction; in that case, the action
	 table is never looked up. *)

      hole

  | None ->

      try
	let target = SymbolMap.find (Symbol.T t) (Lr1.transitions node) in

	(* [node] has a transition to [target]. If [target] has a default
	   reduction on [#], use [ShiftNoDiscard], otherwise [ShiftDiscard]. *)

	match Invariant.has_default_reduction target with
	| Some (_, toks) when TerminalSet.mem Terminal.sharp toks ->
	    assert (TerminalSet.cardinal toks = 1);
	    encode_ShiftNoDiscard target
	| _ ->
	    encode_ShiftDiscard target

      with Not_found ->
	try

	  (* [node] has a reduction. *)

	  let prod = Misc.single (TerminalMap.find t (Lr1.reductions node)) in
	  encode_Reduce prod

	with Not_found ->

	  (* [node] has no action. *)

	  encode_Fail

(* In the error bitmap and in the action table, the row that corresponds to the
   [#] pseudo-terminal is never accessed. Thus, we do not create this row. This
   does not create a gap in the table, because this is the right-most row. For
   sanity, we check this fact here. *)

let () =
  assert (Terminal.t2i Terminal.sharp = Terminal.n - 1)

(* The goto table. *)

let goto node nt =
  try
    let target = SymbolMap.find (Symbol.N nt) (Lr1.transitions node) in
    encode_Goto target
  with Not_found ->
    encode_NoGoto

(* The error bitmap reflects which entries in the action table are
   [Fail]. Like the action table, it is not accessed when [node] has a
   default reduction. *)

let error node t =
  if action node t = encode_Fail then
    encode_Error
  else
    encode_NoError

(* The default reductions table. *)

let default_reduction node =
  match Invariant.has_default_reduction node with
  | Some (prod, _) ->
      encode_DefRed prod
  | None ->
      encode_NoDefRed

(* Generate the table definitions. *)

let action =
  define_and_measure (
    "action",
    marshal2 "action" Lr1.n (Terminal.n - 1) (
      Lr1.map (fun node ->
	Terminal.mapx (fun t ->
	  action node t
	)
      )
    )
  )

let goto =
  define_and_measure (
    "goto",
    marshal2 "goto" Lr1.n Nonterminal.n (
      Lr1.map (fun node ->
	Nonterminal.map (fun nt ->
	  goto node nt
	)
      )
    )
  )

let error =
  define_and_measure (
    "error",
    ETuple [
      EIntConst (Terminal.n - 1);
      marshal11 (
	List.flatten (
	  Lr1.map (fun node ->
	    Terminal.mapx (fun t ->
	      error node t
	    )
	  )
	)
      )
    ]
  )

let default_reduction =
  define_and_measure (
    "default_reduction",
    marshal1 (
      Lr1.map (fun node ->
	default_reduction node
      )
    )
  )

let lhs =
  define_and_measure (
    "lhs",
    marshal1 (
      Production.map (fun prod ->
	Nonterminal.n2i (Production.nt prod)
      )
    )
  )

let semantic_action =
  define (
    "semantic_action",
    EArray (Production.map semantic_action)
  )

(* ------------------------------------------------------------------------ *)

(* When [--trace] is enabled, we need tables that map terminals and
   productions to strings. *)

let stringwrap f x =
  EStringConst (f x)

let reduce_or_accept prod =
  match Production.classify prod with
  | Some _ ->
      "Accepting"
  | None ->
      "Reducing production " ^ (Production.print prod)

let trace =
  define_and_measure (
    "trace",
    if Settings.trace then
      EData ("Some", [
	ETuple [
	  EArray (Terminal.map (stringwrap Terminal.print));
	  EArray (Production.map (stringwrap reduce_or_accept));
	]
      ])
    else
      EData ("None", [])
  )

(* ------------------------------------------------------------------------ *)

(* Generate the two functions that map a token to its integer code and to
   its semantic value, respectively. *)

let token2terminal =
  destructuretokendef
    "token2terminal"
    tint
    false
    (fun tok -> EIntConst (Terminal.t2i tok))

let token2value =
  destructuretokendef
    "token2value"
    tobj
    true
    (fun tok ->
      ERepr (
	match Terminal.ocamltype tok with
	| None ->
	    EUnit
	| Some _ ->
	    EVar semv
      )
    )

(* ------------------------------------------------------------------------ *)

(* We are now ready to apply the functor [TableInterpreter.Make]. *)

(* The type [token], which was defined at toplevel, must be defined again
   in the functor argument. We would like to write [type token = token], but
   that is not valid ocaml. The usual workaround involves a dummy type. *)

let jeton =
  prefix "jeton"

let tokendef1 = {
  typename = jeton;
  typeparams = [];
  typerhs = TAbbrev ttoken;
  typeconstraint = None;
}

let tokendef2 = {
  typename = "token"; (* not [TokenType.tctoken], as it might carry an undesired prefix *)
  typeparams = [];
  typerhs = TAbbrev (TypApp (jeton, []));
  typeconstraint = None;
}

(* Here is the application of [TableInterpreter.Make]. Note that the
   exception [Error], which is defined at toplevel, is re-defined
   within the functor argument: [exception Error = Error]. *)

let application = {

  modulename =
    interpreter;

  modulerhs =
    MApp (
      MVar make,
      MStruct {
	struct_excdefs = [
	  excredef;
	];
	struct_typedefs = [
	  tokendef2;
	];
	struct_nonrecvaldefs = [
	  token2terminal;
	  define ("error_terminal", EIntConst (Terminal.t2i Terminal.error));
	  token2value;
	  default_reduction;
	  error;
	  action;
	  lhs;
	  goto;
	  semantic_action;
	  define ("recovery", eboolconst Settings.recovery);
	  trace;
	];
      }
    );

}

(* ------------------------------------------------------------------------ *)

(* The client API invokes the interpreter with an appropriate start state. *)

let api : IL.valdef list =

  let lexer = "lexer"
  and lexbuf = "lexbuf" in

  ProductionMap.fold (fun prod state api ->

    let nt : Nonterminal.t =
      match Production.classify prod with
      | Some nt ->
	  nt
      | None ->
	  assert false (* this is a start production *)
    in

    let t : typ =
      match Nonterminal.ocamltype nt with
      | Some t ->
	  TypTextual t
      | None ->
	  assert false (* every start symbol should carry a type *)
    in
    
    define (
      Nonterminal.print true nt,
      EFun (
	[ PVar lexer; PVar lexbuf ],
	EAnnot (
	  EMagic (
	    EApp (
	      EVar entry, [
		EIntConst (Lr1.number state);
		EVar lexer;
		EVar lexbuf
	      ]
	    )
	  ),
	  type2scheme t
	)
      )
    ) ::
    api

  ) Lr1.entry []

(* ------------------------------------------------------------------------ *)

(* Let's put everything together. *)

let program = {

  paramdefs =
    Front.grammar.UnparameterizedSyntax.parameters;

  prologue =
    Front.grammar.UnparameterizedSyntax.preludes;

  excdefs =
    [ excdef ];

  typedefs =
    tokentypedef @
    [ tokendef1 ];

  nonrecvaldefs =
    [ excvaldef ];

  moduledefs =
    [ application ];

  valdefs =
    api;

  postlogue =
    Front.grammar.UnparameterizedSyntax.postludes

}

let () =
  Time.tick "Producing abstract syntax"

end


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

open UnparameterizedSyntax
open Syntax
open Stretch
open Positions

(* ------------------------------------------------------------------------ *)
(* Precedence levels for tokens or pseudo-tokens alike. *)

module TokPrecedence = struct

  (* This set records, on a token by token basis, whether the token's
     precedence level is ever useful. This allows emitting warnings
     about useless precedence declarations. *)

  let ever_useful : StringSet.t ref =
    ref StringSet.empty

  let use id =
    ever_useful := StringSet.add id !ever_useful

  (* This function is invoked when someone wants to consult a token's
     precedence level. This does not yet mean that this level is
     useful, though. Indeed, if it is subsequently compared against
     [UndefinedPrecedence], it will not allow solving a conflict. So,
     in addition to the desired precedence level, we return a delayed
     computation which, when evaluated, records that this precedence
     level was useful. *)

  let levelip id properties =
    lazy (use id), properties.tk_priority

  let leveli id = 
    let properties =
      try
	StringMap.find id Front.grammar.tokens
      with Not_found ->
	assert false (* well-formedness check has been performed earlier *)
    in
    levelip id properties    

  (* This function is invoked after the automaton has been constructed.
     It warns about unused precedence levels. *)

  let diagnostics () =
    StringMap.iter (fun id properties ->
      if not (StringSet.mem id !ever_useful) then
	match properties.tk_priority with
	| UndefinedPrecedence ->
	    ()
	| PrecedenceLevel (_, _, pos1, pos2) ->
	    Error.grammar_warning (Positions.two pos1 pos2)
	      (Printf.sprintf "the precedence level assigned to %s is never useful." id)
    ) Front.grammar.tokens

end

(* ------------------------------------------------------------------------ *)
(* Nonterminals. *)

module Nonterminal = struct

  type t = int

  let n2i i = i

  let compare = (-)

  (* Determine how many nonterminals we have and build mappings
     both ways between names and indices. A new nonterminal is
     created for every start symbol. *)

  let new_start_nonterminals =
    StringSet.fold (fun symbol ss -> (symbol ^ "'") :: ss) Front.grammar.start_symbols []

  let original_nonterminals =
    StringMap.fold (fun nt _ rules -> nt :: rules) Front.grammar.rules []
  
  let start =
    List.length new_start_nonterminals

  let (n : int), (name : string array), (map : int StringMap.t) =
    Misc.index (new_start_nonterminals @ original_nonterminals)

  let () =
    Error.logG 1 (fun f ->
      Printf.fprintf f
	"Grammar has %d nonterminal symbols, among which %d start symbols.\n"
	(n - start) start
    )

  let is_start nt =
    nt < start

  let print normalize nt =
    if normalize then
      Misc.normalize name.(nt)
    else
      name.(nt)

  let lookup name =
    StringMap.find name map

  let positions nt =
    (StringMap.find (print false nt) Front.grammar.rules).positions

  let iter f =
    Misc.iteri n f

  let fold f accu =
    Misc.foldi n f accu

  let map f =
    Misc.mapi n f

  let iterx f =
    for nt = start to n - 1 do
      f nt
    done

  let foldx f accu =
    Misc.foldij start n f accu

  let ocamltype nt =
    assert (not (is_start nt));
    try
      Some (StringMap.find (print false nt) Front.grammar.types)
    with Not_found ->
      None

  let tabulate f =
    Array.get (Array.init n f)

end

(* Sets and maps over nonterminals, used only below. *)

module NonterminalMap = Patricia.Big

module NonterminalSet = Patricia.Big.Domain

(* ------------------------------------------------------------------------ *)
(* Terminals. *)

module Terminal = struct

  type t = int

  let t2i i = i

  let compare = (-)

  let equal (tok1 : t) (tok2 : t) =
    tok1 = tok2

  (* Determine how many terminals we have and build mappings
     both ways between names and indices. A new terminal "#"
     is created. A new terminal "error" is created. The fact
     that the integer code assigned to the "error" pseudo-terminal
     is the last one is exploited in the table-based back-end.

     Pseudo-tokens (used in %prec declarations, but never
     declared using %token) are filtered out. *)

  let (n : int), (name : string array), (map : int StringMap.t) =
    let tokens = 
      StringMap.fold (fun token properties tokens ->
	if properties.tk_is_declared then token :: tokens else tokens
      ) Front.grammar.tokens []
    in
    match tokens with
    | [] ->
	Error.error [] "no tokens have been declared."
    | _ ->
	Misc.index ("error" :: tokens @ [ "#" ])

  let print tok =
    name.(tok)

  let lookup name =
    StringMap.find name map

  let sharp =
    lookup "#"

  let error =
    lookup "error"

  let pseudo tok =
    (tok = sharp) || (tok = error)

  let token_properties = 
    let not_so_dummy_properties = (* applicable to [error] and [#] *)
      {
	tk_filename      = "__primitives__";
	tk_priority      = UndefinedPrecedence;
	tk_associativity = UndefinedAssoc;
	tk_ocamltype     = None;
	tk_is_declared   = true;
	tk_position      = Positions.dummy;
      }
    in
    Array.init n (fun tok ->
      try 
	 StringMap.find name.(tok) Front.grammar.tokens 
       with Not_found ->
	 assert (tok = sharp || tok = error);
	 not_so_dummy_properties
    )

  let () =
    Error.logG 1 (fun f ->
      Printf.fprintf f "Grammar has %d terminal symbols.\n" (n - 2)
    )

  let precedence_level tok = 
    TokPrecedence.levelip (print tok) token_properties.(tok)

  let associativity tok =
    token_properties.(tok).tk_associativity

  let ocamltype tok =
    token_properties.(tok).tk_ocamltype

  let iter f =
    Misc.iteri n f

  let fold f accu =
    Misc.foldi n f accu

  let map f =
    Misc.mapi n f

  let mapx f =
    assert (sharp = n - 1);
    Misc.mapi (n-1) f

  (* If a token named [EOF] exists, then it is assumed to represent
     ocamllex's [eof] pattern, which means that the lexer may
     eventually produce an infinite stream of [EOF] tokens. This,
     combined with our error recovery mechanism, may lead to
     non-termination. We provide a warning against this somewhat
     obscure situation.

     Relying on the token's name is somewhat fragile, but this saves
     introducing an extra keyword for declaring which token represents
     [eof], and should not introduce much confusion. *)

  let eof =
    try
      Some (lookup "EOF")
    with Not_found ->
      None

end

(* Sets of terminals are used intensively in the LR(1) construction,
   so it is important that they be as efficient as possible. *)

module TerminalSet = struct

  include CompressedBitSet 

  let print toks =
    let _, accu =
      fold (fun tok (first, accu) ->
	false,
	if first then
          accu ^ (Terminal.print tok)
	else
	  accu ^ " " ^ (Terminal.print tok)
    ) toks (true, "") in
    accu

  let universe =
    remove Terminal.sharp (
      remove Terminal.error (
        Terminal.fold add empty
      )
    )

end

(* Maps over terminals. *)

module TerminalMap = Patricia.Big

(* ------------------------------------------------------------------------ *)
(* Symbols. *)

module Symbol = struct

  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  let compare sym1 sym2 =
    match sym1, sym2 with
    | N nt1, N nt2 ->
	Nonterminal.compare nt1 nt2
    | T tok1, T tok2 ->
	Terminal.compare tok1 tok2
    | N _, T _ ->
	1
    | T _, N _ ->
	-1

  let equal sym1 sym2 =
    compare sym1 sym2 = 0

  let rec lequal syms1 syms2 =
    match syms1, syms2 with
    | [], [] ->
	true
    | sym1 :: syms1, sym2 :: syms2 ->
	equal sym1 sym2 && lequal syms1 syms2
    | _ :: _, []
    | [], _ :: _ ->
	false

  let print = function
    | N nt ->
	Nonterminal.print false nt
    | T tok ->
	Terminal.print tok

  let nonterminal = function
    | T _ ->
	false
    | N _ ->
	true

  (* Printing an array of symbols. [offset] is the start offset -- we
     print everything to its right. [dot] is the dot offset -- we
     print a dot at this offset, if we find it. *)

  let printaod offset dot symbols =
    let buffer = Buffer.create 512 in
    let length = Array.length symbols in
    for i = offset to length do
      if i = dot then
	Buffer.add_string buffer ". ";
      if i < length then begin
	Buffer.add_string buffer (print symbols.(i));
	Buffer.add_char buffer ' '
      end
    done;
    Buffer.contents buffer

  let printao offset symbols =
    printaod offset (-1) symbols

  let printa symbols =
    printao 0 symbols

  let printl symbols =
    printa (Array.of_list symbols)

  let lookup name =
    try
      T (Terminal.lookup name)
    with Not_found ->
      try
	N (Nonterminal.lookup name)
      with Not_found ->
	assert false (* well-formedness check has been performed earlier *)

end

(* Sets of symbols. *)

module SymbolSet = Set.Make(Symbol)

(* Maps over symbols. *)

module SymbolMap = struct

  include Map.Make(Symbol)

  let domain m =
    fold (fun symbol _ accu ->
      symbol :: accu
    ) m []

  let purelynonterminal m =
    fold (fun symbol _ accu ->
      accu && Symbol.nonterminal symbol
    ) m true

end

(* ------------------------------------------------------------------------ *)
(* Productions. *)

module Production = struct

  type index =
      int

  (* Create an array of productions. Record which productions are
     associated with every nonterminal. A new production S' -> S
     is created for every start symbol S. It is known as a
     start production. *)

  let n : int =
    let n = StringMap.fold (fun _ { branches = branches } n ->
      n + List.length branches
    ) Front.grammar.rules 0 in
    Error.logG 1 (fun f -> Printf.fprintf f "Grammar has %d productions.\n" n);
    n + StringSet.cardinal Front.grammar.start_symbols

  let p2i prod =
    prod

  let i2p prod =
    assert (prod >= 0 && prod < n);
    prod

  let table : (Nonterminal.t * Symbol.t array) array =
    Array.make n (-1, [||])

  let identifiers : identifier array array =
    Array.make n [||]

  let used : bool array array =
    Array.make n [||]

  let actions : action option array =
    Array.make n None

  let ntprods : (int * int) array =
    Array.make Nonterminal.n (-1, -1)

  let positions : Positions.t list array =
    Array.make n []

  let (start : int),
      (startprods : index NonterminalMap.t) =
    StringSet.fold (fun nonterminal (k, startprods) ->
      let nt = Nonterminal.lookup nonterminal
      and nt' = Nonterminal.lookup (nonterminal ^ "'") in
      table.(k) <- (nt', [| Symbol.N nt |]);
      identifiers.(k) <- [| "_1" |];
      used.(k) <- [| true |];
      ntprods.(nt') <- (k, k+1);
      positions.(k) <- Nonterminal.positions nt;
      k+1,
      NonterminalMap.add nt k startprods
    ) Front.grammar.start_symbols (0, NonterminalMap.empty)

  let prec_decl : symbol located option array = 
    Array.make n None

  let reduce_precedence : precedence_level array = 
    Array.make n UndefinedPrecedence

  let (_ : int) = StringMap.fold (fun nonterminal { branches = branches } k ->
    let nt = Nonterminal.lookup nonterminal in
    let k' = List.fold_left (fun k branch ->
      let action = branch.action
      and sprec = branch.branch_shift_precedence 
      and rprec = branch.branch_reduce_precedence in	
      let symbols = Array.of_list branch.producers in
      table.(k) <- (nt, Array.map (fun (v, _) -> Symbol.lookup v) symbols);
      identifiers.(k) <- Array.mapi (fun i (_, ido) ->
	match ido with
	| None ->
	    (* Symbols for which no name was chosen will be represented
	       by variables named _1, _2, etc. *)
	    Printf.sprintf "_%d" (i + 1)
        | Some id ->
	    (* Symbols for which a name was explicitly chosen will be
	       known by that name in semantic actions. *)
	    id
      ) symbols;
      used.(k) <- Array.mapi (fun i (_, ido) ->
	match ido with
	| None ->
	    (* A symbol referred to as [$i] is used if and only if the
	       [$i] keyword appears in the semantic action. *)
            Action.has_dollar (i + 1) action
	| Some _ ->
	    (* A symbol referred to via a name is considered used.
	       This is a conservative approximation. *)
            true
      ) symbols;
      actions.(k) <- Some action;
      reduce_precedence.(k) <- rprec;
      prec_decl.(k) <- sprec;
      positions.(k) <- [ branch.branch_position ];
      k+1
    ) k branches in
    ntprods.(nt) <- (k, k');
    k'
  ) Front.grammar.rules start

  (* Iteration over the productions associated with a specific
     nonterminal. *)

  let iternt nt f =
    let k, k' = ntprods.(nt) in
    for prod = k to k' - 1 do
      f prod
    done

  let foldnt (nt : Nonterminal.t) (accu : 'a) (f : index -> 'a -> 'a) : 'a =
    let k, k' = ntprods.(nt) in
    let rec loop accu prod =
      if prod < k' then
	loop (f prod accu) (prod + 1)
      else
	accu
    in
    loop accu k

  (* Accessors. *)

  let def prod =
    table.(prod)

  let nt prod =
    let nt, _ = table.(prod) in
    nt

  let rhs prod =
    let _, rhs = table.(prod) in
    rhs

  let length prod =
    Array.length (rhs prod)

  let identifiers prod =
    identifiers.(prod)

  let used prod =
    used.(prod)

  let is_start prod =
    prod < start

  let classify prod =
    if is_start prod then
      match (rhs prod).(0) with
      | Symbol.N nt ->
	  Some nt
      | Symbol.T _ ->
	  assert false
    else
      None

  let action prod =
    match actions.(prod) with
    | Some action ->
	action
    | None ->
	(* Start productions have no action. *)
	assert (is_start prod);
	assert false

  let positions prod =
    positions.(prod)

  let startsymbol2startprod nt =
    try
      NonterminalMap.find nt startprods
    with Not_found ->
      assert false (* [nt] is not a start symbol *)

  (* Iteration. *)

  let iter f =
    Misc.iteri n f

  let fold f accu =
    Misc.foldi n f accu

  let map f =
    Misc.mapi n f

  let iterx f =
    for prod = start to n - 1 do
      f prod
    done

  let foldx f accu =
    Misc.foldij start n f accu

  (* Printing a production. *)

  let print prod =
    assert (not (is_start prod));
    let nt, rhs = table.(prod) in
    Printf.sprintf "%s -> %s" (Nonterminal.print false nt) (Symbol.printao 0 rhs)

  (* Tabulation. *)

  let tabulate f =
    Misc.tabulateb n f

  (* This array allows recording, on a production by production basis,
     whether the production's shift precedence is ever useful. This
     allows emitting warnings about useless %prec declarations. *)

  let prec_decl_ever_useful =
    Array.make n false

  let consult_prec_decl prod =
    lazy (prec_decl_ever_useful.(prod) <- true),
    prec_decl.(prod)

  let diagnostics () =
    iterx (fun prod ->
      if not prec_decl_ever_useful.(prod) then
	match prec_decl.(prod) with
	| None ->
	    ()
	| Some id ->
	    Error.grammar_warning [Positions.position id] "this %prec declaration is never useful."
    )

  (* Determining the precedence level of a production. If no %prec
     declaration was explicitly supplied, it is the precedence level
     of the rightmost terminal symbol in the production's right-hand
     side. *)

  type production_level =
    | PNone
    | PRightmostToken of Terminal.t
    | PPrecDecl of symbol

  let rightmost_terminal prod =
    Array.fold_left (fun accu symbol ->
      match symbol with
      | Symbol.T tok ->
	  PRightmostToken tok
      | Symbol.N _ ->
	  accu
    ) PNone (rhs prod)

  let combine e1 e2 =
    lazy (Lazy.force e1; Lazy.force e2)

  let shift_precedence prod =
    let fact1, prec_decl = consult_prec_decl prod in
    let oterminal =
      match prec_decl with
      | None ->
	  rightmost_terminal prod
      | Some { value = terminal } ->
	  PPrecDecl terminal
    in
    match oterminal with
    | PNone ->
	fact1, UndefinedPrecedence
    | PRightmostToken tok ->
	let fact2, level = Terminal.precedence_level tok in
	combine fact1 fact2, level
    | PPrecDecl id ->
	let fact2, level = TokPrecedence.leveli id  in
	combine fact1 fact2, level

end

(* ------------------------------------------------------------------------ *)
(* Maps over productions. *)

module ProductionMap = struct

  include Patricia.Big

  (* Iteration over the start productions only. *)

  let start f =
    Misc.foldi Production.start (fun prod m ->
      add prod (f prod) m
    ) empty

end

(* ------------------------------------------------------------------------ *)
(* Build the grammar's forward and backward reference graphs.

   In the backward reference graph, edges relate each nonterminal [nt]
   to each of the nonterminals whose definition mentions [nt]. The
   reverse reference graph is used in the computation of the nullable,
   nonempty, and FIRST sets.

   The forward reference graph is unused but can be printed on demand. *)

let forward : NonterminalSet.t array =
  Array.create Nonterminal.n NonterminalSet.empty

let backward : NonterminalSet.t array =
  Array.create Nonterminal.n NonterminalSet.empty

let () =
  Array.iter (fun (nt1, rhs) ->
    Array.iter (function
      | Symbol.T _ ->
	  ()
      | Symbol.N nt2 ->
	  forward.(nt1) <- NonterminalSet.add nt2 forward.(nt1);
	  backward.(nt2) <- NonterminalSet.add nt1 backward.(nt2)
    ) rhs
  ) Production.table

(* ------------------------------------------------------------------------ *)
(* If requested, dump the forward reference graph. *)

let () =
  if Settings.graph then
    let module P = Dot.Print (struct
      type vertex = Nonterminal.t
      let name nt =
	Printf.sprintf "nt%d" nt
      let successors (f : ?style:Dot.style -> label:string -> vertex -> unit) nt =
	NonterminalSet.iter (fun successor ->
	  f ~label:"" successor
	) forward.(nt)
      let iter (f : ?style:Dot.style -> label:string -> vertex -> unit) =
	Nonterminal.iter (fun nt ->
	  f ~label:(Nonterminal.print false nt) nt
	)
    end) in
    let f = open_out (Settings.base ^ ".dot") in
    P.print f;
    close_out f

(* ------------------------------------------------------------------------ *)
(* Generic support for fixpoint computations.

   A fixpoint computation associates a property with every nonterminal.
   A monotone function tells how properties are computed. [compute nt]
   updates the property associated with nonterminal [nt] and returns a
   flag that tells whether the property actually needed an update. The
   state of the computation is maintained entirely inside [compute] and
   is invisible here.

   Whenever a property of [nt] is updated, the properties of the
   terminals whose definitions depend on [nt] are updated. The
   dependency graph must be explicitly supplied. *)

let fixpoint (dependencies : NonterminalSet.t array) (compute : Nonterminal.t -> bool) : unit =
  let queue : Nonterminal.t Queue.t = Queue.create () in
  let onqueue : bool array = Array.make Nonterminal.n true in
  for i = 0 to Nonterminal.n - 1 do
    Queue.add i queue
  done;
  Misc.qiter (fun nt ->
    onqueue.(nt) <- false;
    let changed = compute nt in
    if changed then
      NonterminalSet.iter (fun nt ->
	if not onqueue.(nt) then begin
	  Queue.add nt queue;
	  onqueue.(nt) <- true
	end
      ) dependencies.(nt)
  ) queue

(* ------------------------------------------------------------------------ *)
(* Compute which nonterminals are nonempty, that is, recognize a
   nonempty language. Also, compute which nonterminals are
   nullable. The two computations are almost identical. The only
   difference is in the base case: a single terminal symbol is not
   nullable, but is nonempty. *)

let compute (basecase : bool) : (bool array) * (Symbol.t -> bool) =
  let property : bool array =
    Array.make Nonterminal.n false
  in
  let symbol_has_property = function
    | Symbol.T _ ->
	basecase
    | Symbol.N nt ->
	property.(nt)
  in
  fixpoint backward (fun nt ->
    if property.(nt) then
      false (* no change *)
    else
      (* disjunction over all productions for this nonterminal *)
      let updated = Production.foldnt nt false (fun prod accu ->
	accu ||
	let rhs = Production.rhs prod in
	(* conjunction over all symbols in the right-hand side *)
	Array.fold_left (fun accu symbol ->
	  accu && symbol_has_property symbol
	) true rhs
      ) in
      property.(nt) <- updated;
      updated
  );
  property, symbol_has_property

let () =
  let nonempty, _ = compute true in
  for nt = Nonterminal.start to Nonterminal.n - 1 do
    if not nonempty.(nt) then
      Error.grammar_warning
	(Nonterminal.positions nt)
	(Printf.sprintf "%s generates the empty language." (Nonterminal.print false nt))
  done

let (nullable : bool array), (nullable_symbol : Symbol.t -> bool) =
  compute false

(* ------------------------------------------------------------------------ *)
(* Compute FIRST sets. *)

let first =
  Array.make Nonterminal.n TerminalSet.empty

let first_symbol = function
  | Symbol.T tok ->
      TerminalSet.singleton tok
  | Symbol.N nt ->
      first.(nt)

let nullable_first_rhs (rhs : Symbol.t array) (i : int) : bool * TerminalSet.t =
  let length = Array.length rhs in
  assert (i <= length);
  let rec loop i toks =
    if i = length then
      true, toks
    else
      let symbol = rhs.(i) in
      let toks = TerminalSet.union (first_symbol symbol) toks in
      if nullable_symbol symbol then
	loop (i+1) toks
      else
	false, toks
  in
  loop i TerminalSet.empty

let () =
  fixpoint backward (fun nt ->
    let original = first.(nt) in
    (* union over all productions for this nonterminal *)
    let updated = Production.foldnt nt TerminalSet.empty (fun prod accu ->
      let rhs = Production.rhs prod in
      let _, toks = nullable_first_rhs rhs 0 in
      TerminalSet.union toks accu
    ) in
    first.(nt) <- updated;
    TerminalSet.compare original updated <> 0
  )

(* ------------------------------------------------------------------------ *)
(* Dump the analysis results. *)

let () =
  Error.logG 2 (fun f ->
    for nt = 0 to Nonterminal.n - 1 do
      Printf.fprintf f "nullable(%s) = %b\n"
	(Nonterminal.print false nt)
	nullable.(nt)
    done;
    for nt = 0 to Nonterminal.n - 1 do
      Printf.fprintf f "first(%s) = %s\n"
	(Nonterminal.print false nt)
	(TerminalSet.print first.(nt))
    done
  )

let () =
  Time.tick "Analysis of the grammar"

(* ------------------------------------------------------------------------ *)
(* Compute FOLLOW sets. Unnecessary for us, but requested by a user. Also,
   this is useful for the SLR(1) test. Thus, we perform this analysis only
   on demand. *)

let follow : TerminalSet.t array Lazy.t =
  lazy (

    let follow =
      Array.make Nonterminal.n TerminalSet.empty

    and forward : NonterminalSet.t array =
      Array.create Nonterminal.n NonterminalSet.empty

    and backward : NonterminalSet.t array =
      Array.create Nonterminal.n NonterminalSet.empty

    in

    (* Iterate over all productions. *)
    Array.iter (fun (nt1, rhs) ->
      (* Iterate over all nonterminal symbols [nt2] in the right-hand side. *)
      Array.iteri (fun i symbol ->
	match symbol with
	| Symbol.T _ ->
	    ()
	| Symbol.N nt2 ->
	    let nullable, first = nullable_first_rhs rhs (i+1) in
	    (* The FIRST set of the remainder of the right-hand side
	       contributes to the FOLLOW set of [nt2]. *)
	    follow.(nt2) <- TerminalSet.union first follow.(nt2);
	    (* If the remainder of the right-hand side is nullable,
	       FOLLOW(nt1) contributes to FOLLOW(nt2). *)
	    if nullable then begin
	      forward.(nt1) <- NonterminalSet.add nt2 forward.(nt1);
	      backward.(nt2) <- NonterminalSet.add nt1 backward.(nt2)
	    end
      ) rhs
    ) Production.table;

    (* The fixpoint computation used here is not the most efficient
       algorithm -- one could do better by first collapsing the
       strongly connected components, then walking the graph in
       topological order. But this will do. *)

    fixpoint forward (fun nt ->
      let original = follow.(nt) in
      (* union over all contributors *)
      let updated = NonterminalSet.fold (fun nt' accu ->
	TerminalSet.union follow.(nt') accu
      ) backward.(nt) original in
      follow.(nt) <- updated;
      TerminalSet.compare original updated <> 0
    );

    follow

  )

(* Define an accessor that triggers the computation of the FOLLOW sets
   if it has not been performed already. *)

let follow nt =
  (Lazy.force follow).(nt)

(* At log level 2, display the FOLLOW sets. *)

let () =
  Error.logG 2 (fun f ->
    for nt = 0 to Nonterminal.n - 1 do
      Printf.fprintf f "follow(%s) = %s\n"
	(Nonterminal.print false nt)
	(TerminalSet.print (follow nt))
    done
  )

(* ------------------------------------------------------------------------ *)
(* Provide explanations about FIRST sets. *)

(* The idea is to explain why a certain token appears in the FIRST set
   for a certain sequence of symbols. Such an explanation involves
   basic assertions of the form (i) symbol N is nullable and (ii) the
   token appears in the FIRST set for symbol N. We choose to take
   these basic facts for granted, instead of recursively explaining
   them, so as to keep explanations short. *)

(* We first produce an explanation in abstract syntax, then
   convert it to a human-readable string. *)

type explanation =
  | EObvious                                 (* sequence begins with desired token *)
  | EFirst of Terminal.t * Nonterminal.t     (* sequence begins with a nonterminal that produces desired token *)
  | ENullable of Symbol.t list * explanation (* sequence begins with a list of nullable symbols and ... *)

let explain (tok : Terminal.t) (rhs : Symbol.t array) (i : int) =
  let length = Array.length rhs in
  let rec loop i =
    assert (i < length);
    let symbol = rhs.(i) in
    match symbol with
    | Symbol.T tok' ->
	assert (Terminal.equal tok tok');
	EObvious
    | Symbol.N nt ->
	if TerminalSet.mem tok first.(nt) then
	  EFirst (tok, nt)
	else begin
	  assert nullable.(nt);
	  match loop (i + 1) with
	  | ENullable (symbols, e) ->
	      ENullable (symbol :: symbols, e)
	  | e ->
	      ENullable ([ symbol ], e)
	end
  in
  loop i

let rec convert = function
  | EObvious ->
      ""
  | EFirst (tok, nt) ->
      Printf.sprintf "%s can begin with %s"
	(Nonterminal.print false nt)
	(Terminal.print tok)
  | ENullable (symbols, e) ->
      let e = convert e in
      Printf.sprintf "%scan vanish%s%s"
	(Symbol.printl symbols)
	(if e = "" then "" else " and ")
	e

(* ------------------------------------------------------------------------ *)
(* Package the analysis results. *)

module Analysis = struct

  let nullable_first_rhs = nullable_first_rhs

  let explain_first_rhs (tok : Terminal.t) (rhs : Symbol.t array) (i : int) =
    convert (explain tok rhs i)

  let follow = follow

end

(* ------------------------------------------------------------------------ *)
(* Conflict resolution via precedences. *)

module Precedence = struct

  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  type order = Lt | Gt | Eq | Ic

  let precedence_order p1 p2 = 
    match p1, p2 with
      |	UndefinedPrecedence, _
      | _, UndefinedPrecedence -> 
	  Ic

      | PrecedenceLevel (m1, l1, _, _), PrecedenceLevel (m2, l2, _, _) ->
	  if not (Mark.same m1 m2) then
	    Ic
	  else
	    if l1 > l2 then 
	      Gt 
	    else if l1 < l2 then 
	      Lt
	    else 
	      Eq

  let shift_reduce tok prod =
    let fact1, tokp  = Terminal.precedence_level tok
    and fact2, prodp = Production.shift_precedence prod in
    match precedence_order tokp prodp with
   
      (* Our information is inconclusive. Drop [fact1] and [fact2],
	 that is, do not record that this information was useful. *)

    | Ic ->
	DontKnow

      (* Our information is useful. Record that fact by evaluating
	 [fact1] and [fact2]. *)

    | (Eq | Lt | Gt) as c ->
	Lazy.force fact1;
	Lazy.force fact2;
	match c with

	| Ic ->
	    assert false (* already dispatched *)

	| Eq -> 
	    begin
	      match Terminal.associativity tok with
	      | LeftAssoc  -> ChooseReduce
	      | RightAssoc -> ChooseShift
	      | NonAssoc   -> ChooseNeither
	      | _          -> assert false
			      (* If [tok]'s precedence level is defined, then
				 its associativity must be defined as well. *)
	    end

	| Lt ->
	    ChooseReduce

	| Gt ->
	    ChooseShift


  let reduce_reduce prod1 prod2 =
    let rp1 = Production.reduce_precedence.(prod1) 
    and rp2 = Production.reduce_precedence.(prod2) in
    match precedence_order rp1 rp2 with
    | Lt -> 
	Some prod1
    | Gt -> 
	Some prod2
    | Eq -> 
	(* the order is strict except in presence of inlining: 
	   two branches can have the same precedence level when
	   they come from an inlined one. *)
	None
    | Ic -> 
	None

end
  
let diagnostics () =
  TokPrecedence.diagnostics();
  Production.diagnostics()


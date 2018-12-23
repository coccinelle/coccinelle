(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(* This module constructs an LR(1) automaton by following Pager's method, that
   is, by merging states on the fly when they are weakly compatible. *)

(* ------------------------------------------------------------------------ *)
(* Run the SLR(1) check first. *)

let () =
  Slr.check()

(* ------------------------------------------------------------------------ *)
(* Nodes. *)

type node = {

    (* A node number, assigned during construction. *)

    raw_number: int;

    (* A node number, assigned after conflict resolution has taken
       place and after inacessible nodes have been removed. This
       yields sequential numbers, from the client's point of view. *)

    mutable number: int;

    (* Each node is associated with a state. This state can change
       during construction as nodes are merged. *)

    mutable state: Lr0.lr1state;

    (* Each node carries information about its outgoing transitions
       and about its reductions. *)

    mutable transitions: node SymbolMap.t;
    mutable reductions: Production.index list TerminalMap.t;

    (* Tokens for which there are several possible behaviors are
       conflict tokens. *)

    mutable conflict_tokens: TerminalSet.t;

    (* Transitions are also stored in reverse, so as to allow reverse
       traversals of the automaton. *)

    mutable predecessors: node list;

    (* Transient marks are used during construction and traversal. *)

    mutable mark: Mark.t;

    (* (New as of 2012/01/23.) This flag records whether a shift/reduce
       conflict in this node was solved in favor of neither (%nonassoc).
       This is later used to forbid a default reduction at this node. *)

    mutable forbid_default_reduction: bool;

  }

module Node = struct
  type t = node
  let compare node1 node2 =
    node1.number - node2.number
end

module NodeSet =
  Set.Make (Node)

module NodeMap =
  Map.Make (Node)

(* ------------------------------------------------------------------------ *)

(* Output debugging information if [--follow-construction] is enabled. *)

let follow_transition (again : bool) (source : node) (symbol : Symbol.t) (state : Lr0.lr1state) =
  if Settings.follow then
    Printf.fprintf stderr
      "%s transition out of state r%d along symbol %s.\nProposed target state:\n%s"
      (if again then "Re-examining" else "Examining")
      source.raw_number
      (Symbol.print symbol)
      (Lr0.print_closure "" state)

let follow_state (msg : string) (node : node) (print : bool) =
  if Settings.follow then
    Printf.fprintf stderr
      "%s: r%d.\n%s\n"
      msg
      node.raw_number
      (if print then Lr0.print_closure "" node.state else "")

(* ------------------------------------------------------------------------ *)

(* The following two mutually recursive functions are invoked when the state
   associated with an existing node grows. The node's descendants are examined
   and grown into a fixpoint is reached.

   This work is performed in an eager manner: we do not attempt to build any
   new transitions until all existing nodes have been suitably grown. Indeed,
   building new transitions requires making merging decisions, and such
   decisions cannot be made on a sound basis unless all existing nodes have
   been suitably grown. Otherwise, one could run into a dead end where two
   successive, incompatible merging decisions are made, because the
   consequences of the first decision (growing descendant nodes) were not made
   explicit before the second decision was taken. This was a bug in versions
   of Menhir ante 20070520.

   Although I wrote this code independently, I later found out that it seems
   quite similar to the code in Karl Schimpf's Ph.D. thesis (1981), page 35.

   It is necessary that all existing transitions be explicit before the [grow]
   functions are called. In other words, if it has been decided that there will
   be a transition from [node1] to [node2], then [node1.transitions] must be
   updated before [grow] is invoked. *)

(* [grow node state] grows the existing node [node], if necessary, so that its
   associated state subsumes [state]. If this represents an actual (strict)
   growth, then [node]'s descendants are grown as well. *)

let rec grow node state =
  if Lr0.subsume state node.state then
    follow_state "Target state is unaffected" node false
   else begin

     (* In versions of Menhir prior to June 2008, I wrote this:

          If I know what I am doing, then the new state that is being
          merged into the existing state should be compatible, in
          Pager's sense, with the existing node. In other words,
          compatibility should be preserved through transitions.

        and the code contained this assertion:

          assert (Lr0.compatible state node.state);
          assert (Lr0.eos_compatible state node.state);

        However, this was wrong. See, for instance, the sample grammars
        cocci.mly and boris-mini.mly. The problem is particularly clearly
        apparent in boris-mini.mly, where it only involves inclusion of
        states -- the definition of Pager's weak compatibility does not
        enter the picture. Here is, roughly, what is going on.

        Assume we have built some state A, which, along some symbol S,
        has a transition to itself. This means, in fact, that computing
        the successor of A along S yields a *subset* of A, that is,
        succ(A, S) <= A.

        Then, we wish to build a new state A', which turns out to be a
        superset of A, so we decide to grow A. (The fact that A is a
        subset of A' implies that A and A' are Pager-compatible.) As
        per the code below, we immediately update the state A in place,
        to become A'. Then, we inspect the transition along symbol S.
        We find that the state succ(A', S) must be merged into A'.

        In this situation, the assertions above require succ(A', S)
        to be compatible with A'. However, this is not necessarily
        the case. By monotonicity of succ, we do have succ(A, S) <=
        succ(A', S). But nothing says that succ(A', S) are related
        with respect to inclusion, or even Pager-compatible. The
        grammar in boris-mini.mly shows that they are not.

     *)

    (* Grow [node]. *)

    node.state <- Lr0.union state node.state;
    follow_state "Growing existing state" node true;

    (* Grow [node]'s successors. *)

    grow_successors node

  end

(* [grow_successors node] grows [node]'s successors. *)

(* Note that, if there is a cycle in the graph, [grow_successors] can be
   invoked several times at a single node [node], with [node.state] taking on
   a new value every time. In such a case, this code should be correct,
   although probably not very efficient. *)

and grow_successors node =
  SymbolMap.iter (fun symbol (successor_node : node) ->
    let successor_state = Lr0.transition symbol node.state in
    follow_transition true node symbol successor_state;
    grow successor_node successor_state
  ) node.transitions

(* ------------------------------------------------------------------------ *)

(* Data structures maintained during the construction of the automaton. *)

(* A queue of pending nodes, whose outgoing transitions have not yet
   been built. *)

let queue : node Queue.t =
  Queue.create()

(* A mapping of LR(0) node numbers to lists of nodes. This allows us to
   efficiently find all existing nodes that are core-compatible with a
   newly found state. *)

let map : node list array =
  Array.make Lr0.n []

(* A counter that allows assigning raw numbers to nodes. *)

let num =
  ref 0

(* ------------------------------------------------------------------------ *)

(* [create state] creates a new node that stands for the state [state].
   It is expected that [state] does not subsume, and is not subsumed by,
   any existing state. *)

let create (state : Lr0.lr1state) : node =

  (* Allocate a new node. *)

  let node = {
    state = state;
    transitions = SymbolMap.empty;
    reductions = TerminalMap.empty;
    conflict_tokens = TerminalSet.empty;
    raw_number = Misc.postincrement num;
    number = 0; (* temporary placeholder *)
    mark = Mark.none;
    predecessors = [];
    forbid_default_reduction = false;
  } in

  (* Update the mapping of LR(0) cores to lists of nodes. *)

  let k = Lr0.core state in
  assert (k < Lr0.n);
  map.(k) <- node :: map.(k);

  (* Enqueue this node for further examination. *)

  Queue.add node queue;

  (* Debugging output. *)

  follow_state "Creating a new state" node false;

  (* Return the freshly created node. *)

  node

(* ------------------------------------------------------------------------ *)

(* Materializing a transition turns its target state into a (fresh or
   existing). There are three scenarios: the proposed new state can be
   subsumed by an existing state, compatible with an existing state, or
   neither. *)

exception Subsumed of node

exception Compatible of node

let materialize (source : node) (symbol : Symbol.t) (target : Lr0.lr1state) : unit =
  try

    (* Debugging output. *)

    follow_transition false source symbol target;

    (* Find all existing core-compatible states. *)

    let k = Lr0.core target in
    assert (k < Lr0.n);
    let similar = map.(k) in

    (* Check whether we need to create a new node or can reuse an existing
       state. *)

    (* 20120525: the manner in which this check is performed depends on
       [Settings.construction_mode]. There are now three modes. *)

    (* 20150204: there are now four modes. *)

    begin match Settings.construction_mode with
    | Settings.ModeCanonical ->

        (* In a canonical automaton, two states can be merged only if they
           are identical. *)

        List.iter (fun node ->
          if Lr0.subsume target node.state &&
             Lr0.subsume node.state target then
            raise (Subsumed node)
        ) similar

    | Settings.ModeInclusionOnly
    | Settings.ModePager ->

        (* A more aggressive approach is to take subsumption into account:
           if the new candidate state is a subset of an existing state,
           then no new node needs to be created. Furthermore, the existing
           state does not need to be enlarged. *)

        (* 20110124: require error compatibility in addition to subsumption. *)

        List.iter (fun node ->
          if Lr0.subsume target node.state &&
             Lr0.error_compatible target node.state then
            raise (Subsumed node)
        ) similar

    | Settings.ModeLALR ->
        ()

    end;

    begin match Settings.construction_mode with
    | Settings.ModeCanonical
    | Settings.ModeInclusionOnly ->
        ()

    | Settings.ModePager ->

        (* One can be even more aggressive and check whether the existing state is
           compatible, in Pager's sense, with the new state. If so, there is no
           need to create a new state: just merge the new state into the existing
           one. The result is a state that may be larger than each of the two
           states that have been merged. *)

        (* 20110124: require error compatibility in addition to the existing
           compatibility criteria. *)

        List.iter (fun node ->
          if Lr0.compatible target node.state &&
             Lr0.eos_compatible target node.state &&
             Lr0.error_compatible target node.state then
            raise (Compatible node)
        ) similar

    | Settings.ModeLALR ->

        (* In LALR mode, as soon as there is one similar state -- i.e. one
           state that shares the same LR(0) core -- we merge the new state
           into the existing one. *)
        List.iter (fun node ->
          raise (Compatible node)
        ) similar

    end;

    (* The above checks have failed. Create a new node. Two states that are in
       the subsumption relation are also compatible. This implies that the
       newly created node does not subsume any existing states. *)

    source.transitions <- SymbolMap.add symbol (create target) source.transitions

  with

  | Subsumed node ->

      (* Join an existing target node. *)

      follow_state "Joining existing state" node false;
      source.transitions <- SymbolMap.add symbol node source.transitions

  | Compatible node ->

      (* Join and grow an existing target node. It seems important that the
         new transition is created before [grow_successors] is invoked, so
         that all transition decisions made so far are explicit. *)

      node.state <- Lr0.union target node.state;
      follow_state "Joining and growing existing state" node true;
      source.transitions <- SymbolMap.add symbol node source.transitions;
      grow_successors node

(* ------------------------------------------------------------------------ *)

(* The actual construction process. *)

(* Populate the queue with the start nodes and store them in an array. *)

let entry : node ProductionMap.t =
  ProductionMap.map (fun (k : Lr0.node) ->
    create (Lr0.start k)
  ) Lr0.entry

(* Pick a node in the queue, that is, a node whose transitions have not yet
   been built. Build these transitions, and continue. *)

(* Note that building a transition can cause existing nodes to grow, so
   [node.state] is not necessarily invariant throughout the inner loop. *)

let () =
  Misc.qiter (fun node ->
    List.iter (fun symbol ->
      materialize node symbol (Lr0.transition symbol node.state)
    ) (Lr0.outgoing_symbols (Lr0.core node.state))
  ) queue

(* Record how many nodes were constructed. *)

let n =
  !num

let () =
  Error.logA 1 (fun f -> Printf.fprintf f "Built an LR(1) automaton with %d states.\n" !num)

(* ------------------------------------------------------------------------ *)

(* A mapping of symbols to lists of nodes that admit this incoming
   symbol. This mapping is constructed by [visit] below. *)

let incoming : node list SymbolMap.t ref =
  ref SymbolMap.empty

let lookup_incoming symbol =
  try
    SymbolMap.find symbol !incoming
  with Not_found ->
    []

let record_incoming osymbol target =
  Option.iter (fun symbol ->
    let targets = lookup_incoming symbol in
    incoming := SymbolMap.add symbol (target :: targets) !incoming
  ) osymbol

(* ------------------------------------------------------------------------ *)
(* We now perform one depth-first traversal of the automaton,
   recording predecessor edges, numbering nodes, sorting nodes
   according to their incoming symbol, building reduction tables, and
   finding out which nodes have conflicts. *)

(* A count of all nodes. *)

let () =
  num := 0

(* A list of all nodes. *)

let nodes : node list ref =
  ref []

(* A list of nodes with conflicts. *)

let conflict_nodes : node list ref =
  ref []

(* Counts of nodes with shift/reduce and reduce/reduce conflicts. *)

let shift_reduce =
  ref 0

let reduce_reduce =
  ref 0

(* Count of the shift/reduce conflicts that could be silently
   resolved. *)

let silently_solved =
  ref 0

(* Go ahead. *)

let () =

  let marked = Mark.fresh() in

  let rec visit osymbol node =
    if not (Mark.same node.mark marked) then begin
      node.mark <- marked;
      nodes := node :: !nodes;
      record_incoming osymbol node;

      (* Number this node. *)

      let number = !num in
      num := number + 1;
      node.number <- number;

      (* Insertion of a new reduce action into the table of reductions. *)

      let addl prod tok reductions =
        let prods =
          try
            TerminalMap.lookup tok reductions
          with Not_found ->
            []
        in
        TerminalMap.add tok (prod :: prods) reductions
      in

      (* Build the reduction table. Here, we gather all potential
         reductions, without attempting to solve shift/reduce
         conflicts on the fly, because that would potentially hide
         shift/reduce/reduce conflicts, which we want to be aware
         of. *)

      let reductions =
        List.fold_left (fun reductions (toks, prod) ->
          TerminalSet.fold (addl prod) toks reductions
        ) TerminalMap.empty (Lr0.reductions node.state)
      in

      (* Detect conflicts. Attempt to solve shift/reduce conflicts
         when unambiguously allowed by priorities. *)

      let has_shift_reduce = ref false
      and has_reduce_reduce = ref false in

      node.reductions <-
        TerminalMap.fold (fun tok prods reductions ->
          if SymbolMap.mem (Symbol.T tok) node.transitions then begin

            (* There is a transition in addition to the reduction(s). We
               have (at least) a shift/reduce conflict. *)

            assert (not (Terminal.equal tok Terminal.sharp));
            match prods with
            | [] ->
                assert false
            | [ prod ] ->
                begin

                  (* This is a single shift/reduce conflict. If priorities tell
                     us how to solve it, we follow that and modify the automaton. *)

                  match Precedence.shift_reduce tok prod with

                  | Precedence.ChooseShift ->

                      (* Suppress the reduce action. *)

                      incr silently_solved;
                      reductions

                  | Precedence.ChooseReduce ->

                      (* Record the reduce action and suppress the shift transition.
                         The automaton is modified in place. This can have the subtle
                         effect of making some nodes unreachable. Any conflicts in these
                         nodes will then be ignored (as they should be). *)

                      incr silently_solved;
                      node.transitions <- SymbolMap.remove (Symbol.T tok) node.transitions;
                      TerminalMap.add tok prods reductions

                  | Precedence.ChooseNeither ->

                      (* Suppress the reduce action and the shift transition. *)

                      incr silently_solved;
                      node.transitions <- SymbolMap.remove (Symbol.T tok) node.transitions;
                      node.forbid_default_reduction <- true;
                      reductions

                  | Precedence.DontKnow ->

                      (* Priorities don't allow concluding. Record the
                         existence of a shift/reduce conflict. *)

                      node.conflict_tokens <- Grammar.TerminalSet.add tok node.conflict_tokens;
                      has_shift_reduce := true;
                      TerminalMap.add tok prods reductions

                end

            | _prod1 :: _prod2 :: _ ->

                (* This is a shift/reduce/reduce conflict. If the priorities
                   are such that each individual shift/reduce conflict is solved
                   in favor of shifting or in favor of neither, then solve the entire
                   composite conflict in the same way. Otherwise, report the conflict. *)

                let choices = List.map (Precedence.shift_reduce tok) prods in

                if List.for_all (fun choice ->
                  match choice with
                  | Precedence.ChooseShift -> true
                  | _ -> false
                ) choices then begin

                  (* Suppress the reduce action. *)

                  silently_solved := !silently_solved + List.length prods;
                  reductions

                end
                else if List.for_all (fun choice ->
                  match choice with
                  | Precedence.ChooseNeither -> true
                  | _ -> false
                ) choices then begin

                  (* Suppress the reduce action and the shift transition. *)

                  silently_solved := !silently_solved + List.length prods;
                  node.transitions <- SymbolMap.remove (Symbol.T tok) node.transitions;
                  reductions

                end
                else begin

                  (* Record a shift/reduce/reduce conflict. Keep all reductions. *)

                  node.conflict_tokens <- Grammar.TerminalSet.add tok node.conflict_tokens;
                  has_shift_reduce := true;
                  has_reduce_reduce := true;
                  TerminalMap.add tok prods reductions

                end

          end
          else
            let () =
              match prods with
              | []
              | [ _ ] ->
                  ()
              | _prod1 :: _prod2 :: _ ->

                  (* There is no transition in addition to the reduction(s). We
                     have a pure reduce/reduce conflict. Do nothing about it at
                     this point. *)

                  node.conflict_tokens <- Grammar.TerminalSet.add tok node.conflict_tokens;
                  has_reduce_reduce := true

            in
            TerminalMap.add tok prods reductions

      ) reductions TerminalMap.empty;

      (* Record statistics about conflicts. *)

      if not (TerminalSet.is_empty node.conflict_tokens) then begin
        conflict_nodes := node :: !conflict_nodes;
        if !has_shift_reduce then
          incr shift_reduce;
        if !has_reduce_reduce then
          incr reduce_reduce
      end;

      (* Continue the depth-first traversal. Record predecessors edges
         as we go. No ancestor appears twice in a list of
         predecessors, because two nodes cannot be related by two
         edges that carry distinct symbols. *)

      SymbolMap.iter (fun symbol son ->
        son.predecessors <- node :: son.predecessors;
        visit (Some symbol) son
      ) node.transitions
    end
  in

  ProductionMap.iter (fun _ node -> visit None node) entry

let nodes =
  List.rev !nodes (* list is now sorted by increasing node numbers *)

let conflict_nodes =
  !conflict_nodes

let () =
  if !silently_solved = 1 then
    Error.logA 1 (fun f -> Printf.fprintf f "One shift/reduce conflict was silently solved.\n")
  else if !silently_solved > 1 then
    Error.logA 1 (fun f -> Printf.fprintf f "%d shift/reduce conflicts were silently solved.\n" !silently_solved);
  if !num < n then
    Error.logA 1 (fun f -> Printf.fprintf f "Only %d states remain after resolving shift/reduce conflicts.\n" !num)

let () =
  Grammar.diagnostics()

let n =
  !num

let forbid_default_reduction node =
  node.forbid_default_reduction

(* ------------------------------------------------------------------------ *)
(* The incoming symbol of a node can be computed by going through its LR(0)
   core. For this reason, we do not need to explicitly record it here. *)

let incoming_symbol node =
  Lr0.incoming_symbol (Lr0.core node.state)

(* ------------------------------------------------------------------------ *)
(* Iteration over all nodes. *)

let fold f accu =
  List.fold_left f accu nodes

let iter f =
  fold (fun () node -> f node) ()

let map f =
  List.map f nodes

let foldx f =
  fold (fun accu node ->
          match incoming_symbol node with
            | None -> accu
            | Some _ -> f accu node)

let iterx f =
  iter (fun node ->
    match incoming_symbol node with
      | None -> ()
      | Some _ -> f node)

(* -------------------------------------------------------------------------- *)
(* Our output channel. *)

let out =
  lazy (open_out (Settings.base ^ ".automaton"))

(* ------------------------------------------------------------------------ *)
(* If requested, dump a verbose description of the automaton. *)

let describe out node =
  Printf.fprintf out "State %d%s:\n%s"
    node.number
    (if Settings.follow then Printf.sprintf " (r%d)" node.raw_number else "")
    (Lr0.print "" node.state);
  SymbolMap.iter (fun symbol node ->
    Printf.fprintf out "-- On %s shift to state %d\n"
      (Symbol.print symbol) node.number
  ) node.transitions;
  TerminalMap.iter (fun tok prods ->
    List.iter (fun prod ->
      (* TEMPORARY factoriser les symboles qui conduisent a reduire une meme production *)
      Printf.fprintf out "-- On %s " (Terminal.print tok);
      match Production.classify prod with
      | Some nt ->
          Printf.fprintf out "accept %s\n" (Nonterminal.print false nt)
      | None ->
          Printf.fprintf out "reduce production %s\n" (Production.print prod)
    ) prods
  ) node.reductions;
  if not (TerminalSet.is_empty node.conflict_tokens) then
    Printf.fprintf out "** Conflict on %s\n" (TerminalSet.print node.conflict_tokens);
  Printf.fprintf out "\n%!"

let () =
  Time.tick "Construction of the LR(1) automaton";
  if Settings.dump then begin
    iter (describe (Lazy.force out));
    Time.tick "Dumping the LR(1) automaton"
  end

(* ------------------------------------------------------------------------ *)
(* [reverse_dfs goal] performs a reverse depth-first search through
   the automaton, starting at node [goal], and marking the nodes
   traversed. It returns a function that tells whether a node is
   marked, that is, whether a path leads from that node to the goal
   node. *)

let reverse_dfs goal =

  let mark = Mark.fresh() in

  let marked node =
    Mark.same node.mark mark
  in

  let rec visit node =
     if not (marked node) then begin
       node.mark <- mark;
       List.iter visit node.predecessors
     end
  in

  visit goal;
  marked

(* ------------------------------------------------------------------------ *)
(* Iterating over all nodes that are targets of edges carrying a
   certain symbol. The sources of the corresponding edges are also
   provided. *)

let targets f accu symbol =
  (* There are no incoming transitions on the start symbols. *)
  let targets = lookup_incoming symbol in
  List.fold_left (fun accu target ->
    f accu target.predecessors target
  ) accu targets

(* ------------------------------------------------------------------------ *)
(* Converting a start node into the single item that it contains. *)

let start2item node =
  let state : Lr0.lr1state = node.state in
  let core : Lr0.node = Lr0.core state in
  let items : Item.Set.t = Lr0.items core in
  assert (Item.Set.cardinal items = 1);
  Item.Set.choose items

(* ------------------------------------------------------------------------ *)
(* Accessors. *)

let number node =
  node.number

let state node =
  node.state

let transitions node =
  node.transitions

let reductions node =
  node.reductions

let conflicts f =
  List.iter (fun node ->
    f node.conflict_tokens node
  ) conflict_nodes

let predecessors node =
  node.predecessors

(* ------------------------------------------------------------------------ *)

(* This inverts a mapping of tokens to productions into a mapping of
   productions to sets of tokens. *)

(* This is needed, in [CodeBackend], to avoid producing two (or more)
   separate branches that call the same [reduce] function. Instead,
   we generate just one branch, guarded by a [POr] pattern. *)

let invert reductions : TerminalSet.t ProductionMap.t =
  TerminalMap.fold (fun tok prods inverse ->
    let prod = Misc.single prods in
    let toks =
      try
        ProductionMap.lookup prod inverse
      with Not_found ->
        TerminalSet.empty
    in
    ProductionMap.add prod (TerminalSet.add tok toks) inverse
  ) reductions ProductionMap.empty

(* ------------------------------------------------------------------------ *)
(* [has_beforeend s] tests whether the state [s] can reduce a production
   whose semantic action uses [$endpos($0)]. Note that [$startpos] and
   [$endpos] have been expanded away already, so we need not worry about
   the fact that (in an epsilon production) they expand to [$endpos($0)]. *)

let has_beforeend node =
  TerminalMap.fold (fun _ prods accu ->
    accu ||
    let prod = Misc.single prods in
    not (Production.is_start prod) &&
    let action = Production.action prod in
    Action.has_beforeend action
  ) (reductions node) false

(* ------------------------------------------------------------------------ *)
(* Computing which terminal symbols a state is willing to act upon.

   One must keep in mind that, due to the merging of states, a state might be
   willing to perform a reduction on a certain token, yet the reduction can
   take us to another state where this token causes an error. In other words,
   the set of terminal symbols that is computed here is really an
   over-approximation of the set of symbols that will not cause an error. And
   there seems to be no way of performing an exact computation, as we would
   need to know not only the current state, but the contents of the stack as
   well. *)

let acceptable_tokens (s : node) =

  (* If this state is willing to act on the error token, ignore it -- we do
     not wish to report that an error would be accepted in this state :-) *)

  let transitions =
    SymbolMap.remove (Symbol.T Terminal.error) (transitions s)
  and reductions =
    TerminalMap.remove Terminal.error (reductions s)
  in

  (* Accumulate the tokens carried by outgoing transitions. *)

  let covered =
    SymbolMap.fold (fun symbol _ covered ->
      match symbol with
      | Symbol.T tok ->
          TerminalSet.add tok covered
      | Symbol.N _ ->
          covered
    ) transitions TerminalSet.empty
  in

  (* Accumulate the tokens that permit reduction. *)

  let covered =
    ProductionMap.fold (fun _ toks covered ->
      TerminalSet.union toks covered
    ) (invert reductions) covered
  in

  (* That's it. *)

  covered

(* ------------------------------------------------------------------------ *)
(* Report statistics. *)

(* Produce the reports. *)

let () =
  if !shift_reduce = 1 then
    Error.grammar_warning [] "one state has shift/reduce conflicts."
  else if !shift_reduce > 1 then
    Error.grammar_warning [] "%d states have shift/reduce conflicts." !shift_reduce;
  if !reduce_reduce = 1 then
    Error.grammar_warning [] "one state has reduce/reduce conflicts."
  else if !reduce_reduce > 1 then
    Error.grammar_warning [] "%d states have reduce/reduce conflicts." !reduce_reduce

(* ------------------------------------------------------------------------ *)

(* For each production, compute where (that is, in which states) this
   production can be reduced. This computation is done AFTER default conflict
   resolution (see below). It is an error to call the accessor function
   [production_where] before default conflict resolution has taken place. *)

let production_where : NodeSet.t ProductionMap.t option ref =
  ref None

let initialize_production_where () =
  production_where := Some (
    fold (fun accu node ->
      TerminalMap.fold (fun _ prods accu ->
        let prod = Misc.single prods in
        let nodes =
          try
            ProductionMap.lookup prod accu
          with Not_found ->
            NodeSet.empty
        in
        ProductionMap.add prod (NodeSet.add node nodes) accu
      ) (reductions node) accu
    ) ProductionMap.empty
  )

let production_where (prod : Production.index) : NodeSet.t =
  match !production_where with
  | None ->
      (* It is an error to call this function before conflict resolution. *)
      assert false
  | Some production_where ->
      try
        (* Production [prod] may be reduced at [nodes]. *)
        let nodes = ProductionMap.lookup prod production_where in
        assert (not (NodeSet.is_empty nodes));
        nodes
      with Not_found ->
        (* The production [prod] is never reduced. *)
        NodeSet.empty

(* ------------------------------------------------------------------------ *)
(* Warn about productions that are never reduced. *)

(* These are productions that can never, ever be reduced, because there is
   no state that is willing to reduce them. There could be other productions
   that are never reduced because the only states that are willing to reduce
   them are unreachable. We do not report those. In fact, through the use of
   the inspection API, it might be possible to bring the automaton into a
   state where one of those productions can be reduced. *)

let warn_about_productions_never_reduced () =
  let count = ref 0 in
  Production.iter (fun prod ->
    if NodeSet.is_empty (production_where prod) then
      match Production.classify prod with
      | Some nt ->
          incr count;
          Error.grammar_warning
            (Nonterminal.positions nt)
            "symbol %s is never accepted." (Nonterminal.print false nt)
      | None ->
          incr count;
          Error.grammar_warning
            (Production.positions prod)
            "production %sis never reduced." (Production.print prod)
  );
  if !count > 0 then
    let plural_mark, be = if !count > 1 then ("s", "are") else ("", "is") in
    Error.grammar_warning []
      "in total, %d production%s %s never reduced." !count plural_mark be

(* ------------------------------------------------------------------------ *)
(* When requested by the code generator, apply default conflict
   resolution to ensure that the automaton is deterministic. *)

(* [best prod prods] chooses which production should be reduced
   among the list [prod :: prods]. It fails if no best choice
   exists. *)

let rec best choice = function
  | [] ->
      choice
  | prod :: prods ->
      match Precedence.reduce_reduce choice prod with
      | Some choice ->
          best choice prods
      | None ->
          (* The cause for not knowing which production is best could be:
             1- the productions originate in different source files;
             2- they are derived, via inlining, from the same production. *)
          Error.signal Error.grammatical_error
            (Production.positions choice @ Production.positions prod)
               "do not know how to resolve a reduce/reduce conflict\n\
                between the following two productions:\n%s\n%s"
                  (Production.print choice)
                  (Production.print prod);
          choice (* dummy *)

(* Go ahead. *)

let default_conflict_resolution () =

  let shift_reduce =
    ref 0
  and reduce_reduce =
    ref 0
  in

  List.iter (fun node ->

    node.reductions <-
      TerminalMap.fold (fun tok prods reductions ->
        try
          let (_ : node) =
            SymbolMap.find (Symbol.T tok) node.transitions
          in
          (* There is a transition at this symbol, so this
             is a (possibly multiway) shift/reduce conflict.
             Resolve in favor of shifting by suppressing all
             reductions. *)
          shift_reduce := List.length prods + !shift_reduce;
          reductions
        with Not_found ->
          (* There is no transition at this symbol. Check
             whether we have multiple reductions. *)
          match prods with
          | [] ->
              assert false
          | [ _ ] ->
              TerminalMap.add tok prods reductions
          | prod :: ((_ :: _) as prods) ->
              (* We have a reduce/reduce conflict. Resolve, if
                 possible, in favor of a single reduction.
                 This reduction must be preferrable to each
                 of the others. *)
              reduce_reduce := List.length prods + !reduce_reduce;
              TerminalMap.add tok [ best prod prods ] reductions

      ) node.reductions TerminalMap.empty

  ) conflict_nodes;

  if !shift_reduce = 1 then
    Error.warning [] "one shift/reduce conflict was arbitrarily resolved."
  else if !shift_reduce > 1 then
    Error.warning [] "%d shift/reduce conflicts were arbitrarily resolved." !shift_reduce;
  if !reduce_reduce = 1 then
    Error.warning [] "one reduce/reduce conflict was arbitrarily resolved."
  else if !reduce_reduce > 1 then
    Error.warning [] "%d reduce/reduce conflicts were arbitrarily resolved." !reduce_reduce;

  (* Now, ensure that states that have a reduce action at the
     pseudo-token "#" have no other action. *)

  let ambiguities =
    ref 0
  in

  fold (fun () node ->

    try
      let prods, reductions = TerminalMap.lookup_and_remove Terminal.sharp node.reductions in
      let prod = Misc.single prods in

      (* This node has a reduce action at "#". Determine whether there
         exist other actions. If there exist any other actions,
         suppress this reduce action, and signal an ambiguity.

         We signal an ambiguity even in the case where all actions at
         this node call for reducing a single production. Indeed, in
         that case, even though we know that this production must be
         reduced, we do not know whether we should first discard the
         current token (and call the lexer). *)

      let has_ambiguity = ref false in
      let toks = ref TerminalSet.empty in

      TerminalMap.iter (fun tok _prods ->
        node.reductions <- reductions;
        has_ambiguity := true;
        toks := TerminalSet.add tok !toks
      ) reductions;

      SymbolMap.iter (fun symbol _ ->
        match symbol with
        | Symbol.N _ ->
            ()
        | Symbol.T tok ->
            node.reductions <- reductions;
            has_ambiguity := true;
            toks := TerminalSet.add tok !toks
      ) node.transitions;

      if !has_ambiguity then begin
        incr ambiguities;
        if Settings.dump then begin
          Printf.fprintf (Lazy.force out)
            "State %d has an end-of-stream conflict. There is a tension between\n\
             (1) %s\n\
             without even requesting a lookahead token, and\n\
             (2) checking whether the lookahead token is %s%s,\n\
             which would require some other action.\n\n"
            (number node)
            (match Production.classify prod with
            | Some nt ->
                Printf.sprintf "accepting %s" (Nonterminal.print false nt)
            | None ->
                Printf.sprintf "reducing production %s" (Production.print prod))
            (if TerminalSet.cardinal !toks > 1 then "one of " else "")
            (TerminalSet.print !toks)
        end
      end

    with Not_found ->
      ()

  ) ();

  if !ambiguities = 1 then
    Error.grammar_warning [] "one state has an end-of-stream conflict."
  else if !ambiguities > 1 then
    Error.grammar_warning [] "%d states have an end-of-stream conflict." !ambiguities;

  (* We can now compute where productions are reduced. *)
  initialize_production_where();
  warn_about_productions_never_reduced()

(* ------------------------------------------------------------------------ *)
(* Extra reductions. *)

(* 2015/10/19 Original implementation. *)
(* 2016/07/13 Use priority levels to choose which productions to reduce
              when several productions are eligible. *)

(* If a state can reduce some productions whose left-hand symbol has been
   marked [%on_error_reduce], and if one such production [prod] is preferable
   to every other (according to the priority rules of [%on_error_reduce]
   declarations), then every error action in this state is replaced with a
   reduction of [prod]. This is done even though this state may have outgoing
   shift transitions: thus, we are forcing one interpretation of the past,
   among several possible interpretations. *)

(* The code below looks like the decision on a default reduction in
   [Default], except we do not impose the absence of outgoing terminal
   transitions. Also, we actually modify the automaton, so the back-ends, the
   reference interpreter, etc., need not be aware of this feature, whereas
   they are aware of default reductions. *)

(* This code can run before we decide on the default reductions; this does
   not affect which default reductions will be permitted. *)

(* This code does not affect which productions can be reduced where. Thus,
   it is OK for it to run after [initialize_production_where()]. *)

(* A count of how many states receive extra reductions through this mechanism. *)

let extra =
  ref 0

(* A count of how many states have more than one eligible production, but one
   is preferable to every other (so priority plays a role). *)

let prioritized =
  ref 0

(* The set of nonterminal symbols in the left-hand side of an extra reduction. *)

let extra_nts =
  ref NonterminalSet.empty

let extra_reductions_in_node node =
  (* Compute the productions which this node can reduce. *)
  let productions : _ ProductionMap.t = invert (reductions node) in
  let prods : Production.index list =
    ProductionMap.fold (fun prod _ prods -> prod :: prods) productions []
  in
  (* Keep only those whose left-hand symbol is marked [%on_error_reduce]. *)
  let prods = List.filter OnErrorReduce.reduce prods in
  (* Check if one of them is preferable to every other one. *)
  match Misc.best OnErrorReduce.preferable prods with
  | None ->
      (* Either no production is marked [%on_error_reduce], or several of them
         are marked and none is preferable. *)
      ()
  | Some prod ->
      let acceptable = acceptable_tokens node in
      (* An extra reduction is possible. Replace every error action with
         a reduction of [prod]. If we replace at least one error action
         with a reduction, update [extra] and [extra_nts]. *)
      let triggered = lazy (
        incr extra;
        if List.length prods > 1 then incr prioritized;
        extra_nts := NonterminalSet.add (Production.nt prod) !extra_nts
      ) in
      Terminal.iter_real (fun tok ->
        if not (TerminalSet.mem tok acceptable) then begin
          node.reductions <- TerminalMap.add tok [ prod ] node.reductions;
          Lazy.force triggered
        end
      )

let extra_reductions () =
  (* Examine every node. *)
  iter (fun node ->
    (* Just like a default reduction, an extra reduction should be forbidden
       (it seems) if [forbid_default_reduction] is set. *)
    if not node.forbid_default_reduction then
      extra_reductions_in_node node
  );
  (* Info message. *)
  if !extra > 0 then
    Error.logA 1 (fun f ->
      Printf.fprintf f "Extra reductions on error were added in %d states.\n" !extra;
      Printf.fprintf f "Priority played a role in %d of these states.\n" !prioritized
    );
  (* Warn about useless %on_error_reduce declarations. *)
  OnErrorReduce.iter (fun nt ->
    if not (NonterminalSet.mem nt !extra_nts) then
      Error.grammar_warning []
        "the declaration %%on_error_reduce %s is never useful."
        (Nonterminal.print false nt)
  )

(* ------------------------------------------------------------------------ *)
(* Define [fold_entry], which in some cases facilitates the use of [entry]. *)

let fold_entry f accu =
  ProductionMap.fold (fun prod state accu ->
    let nt : Nonterminal.t =
      match Production.classify prod with
      | Some nt ->
          nt
      | None ->
          assert false (* this is a start production *)
    in
    let t : Stretch.ocamltype =
      Nonterminal.ocamltype_of_start_symbol nt
    in
    f prod state nt t accu
  ) entry accu

let entry_of_nt nt =
  (* Find the entry state that corresponds to [nt]. *)
  try
    ProductionMap.find (Production.startsymbol2startprod nt) entry
  with Not_found ->
    assert false

exception Found of Nonterminal.t

let nt_of_entry s =
  (* [s] should be an initial state. *)
  assert (incoming_symbol s = None);
  try
    ProductionMap.iter (fun prod entry ->
      if Node.compare s entry = 0 then
        match Production.classify prod with
        | None ->
            assert false
        | Some nt ->
            raise (Found nt)
    ) entry;
    (* This should not happen if [s] is indeed an initial state. *)
    assert false
  with Found nt ->
    nt

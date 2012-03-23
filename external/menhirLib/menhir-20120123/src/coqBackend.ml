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

open Printf
open Grammar

module Run (T: sig end) = struct

  let print_term t =
    assert (not (Terminal.pseudo t));
    sprintf "%s_t" (Terminal.print t)

  let print_nterm nt =
    sprintf "%s_nt" (Nonterminal.print true nt)

  let print_symbol = function
    | Symbol.N nt -> sprintf "NT %s" (print_nterm nt)
    | Symbol.T t -> sprintf "T %s" (print_term t)

  let print_type ty =
    if Settings.coq_no_actions then
      "unit"
    else
      match ty with
        | None -> raise Not_found
        | Some t -> match t with
	    | Stretch.Declared s -> s.Stretch.stretch_content
	    | Stretch.Inferred _ -> assert false (* We cannot infer coq types *)

  let print_nis nis =
    sprintf "Nis_%d" (Lr1.number nis)

  let print_st st =
    match Lr1.incoming_symbol st with
      | Some _ -> sprintf "Ninit %s" (print_nis st)
      | None -> "Init"

  let print_prod p =
    sprintf "Prod_%d" (Production.p2i p)

  let print_pseudoprod p =
    match Production.classify p with
      | None -> sprintf "Some %s" (print_prod p)
      | Some _ -> "None"

  let start_nt =
    match
      ProductionMap.fold (fun _ rhs accu ->
        match accu with
          | None -> (match rhs with [|Symbol.N nt|] -> Some nt | _ -> assert false)
          | _ -> Error.error [] ("Multiple start symbols are not supported "^
                                 "by the coq back-end"))
        (ProductionMap.start Production.rhs)
        None
    with
      | Some start -> start
      | _ -> Error.error [] "There is no start symbol"

  let () =
    if not Settings.coq_no_actions then
      begin
        Nonterminal.iterx (fun nonterminal ->
          match Nonterminal.ocamltype nonterminal with
            | None -> Error.error [] (sprintf "I don't know the type of non-terminal %s"
                                     (Nonterminal.print false nonterminal))
            | Some _ -> ());
        Production.iterx (fun prod ->
          let act =  Production.action prod in
          if Action.has_previouserror act || Action.has_syntaxerror act ||
            Action.has_leftstart act || Action.has_leftend act ||
            Action.use_dollar act then
              Error.error [] ("$previouserror, $syntaxerror, $start, $end, $i are not "^
                             "supported by the coq back-end"))
      end;

    Production.iterx (fun prod ->
      Array.iter (fun symb ->
        match symb with
          | Symbol.T t ->
              if t = Terminal.error then
                Error.error [] "The coq back-end does not support error"
          | _ -> ())
        (Production.rhs prod));

    if Front.grammar.UnparameterizedSyntax.parameters <> [] then
      Error.error [] "The coq back-end does not support %parameter"

  let write_inductive_alphabet f name constrs =
    fprintf f "Inductive %s' :=" name;
    List.iter (fprintf f "\n  | %s") constrs;
    fprintf f ".\n";
    fprintf f "Definition %s := %s'.\n\n" name name;
    if List.length constrs > 0 then
      begin
        let iteri f = ignore (List.fold_left (fun k x -> f k x; succ k) 0 constrs) in
        fprintf f "Open Scope N_scope.\n";
        fprintf f "Program Instance %sNum : Numbered %s :=\n" name name;
        fprintf f "  { injN := fun x => match x return _ with ";
        iteri (fun k constr -> fprintf f "| %s => %d " constr k);
        fprintf f "end;\n";
        fprintf f "    surjN := fun n => match n return _ with ";
        iteri (fprintf f "| %d => %s ");
        fprintf f "| _ => %s end;\n" (List.hd constrs);
        fprintf f "  injN_bound := %d }.\n" (List.length constrs);
        fprintf f "Solve Obligations using (intro x; case x; reflexivity).\n\n";
      end
    else
      begin
        fprintf f "Program Instance %sAlph : Alphabet %s :=\n" name name;
        fprintf f "  { AlphabetComparable := {| compare := fun x y =>\n";
        fprintf f "      match x, y return comparison with end |};\n";
        fprintf f "    AlphabetEnumerable := {| all_list := [] |} }.";
        fprintf f "Solve Obligations using (intro x; case x)."
      end

  let write_terminals f =
    write_inductive_alphabet f "terminal" (
      Terminal.fold (fun t l -> if Terminal.pseudo t then l else print_term t::l)
        []);
    fprintf f "Instance TerminalAlph : Alphabet terminal := _.\n\n"

  let write_nonterminals f =
    write_inductive_alphabet f "nonterminal" (
      Nonterminal.foldx (fun nt l -> (print_nterm nt)::l) []);
    fprintf f "Instance NonTerminalAlph : Alphabet nonterminal := _.\n\n"

  let write_symbol_semantic_type f =
    fprintf f "Definition symbol_semantic_type (s:symbol) : Type:=\n";
    fprintf f "  match s with\n";

    fprintf f "    | T t =>\n";
    fprintf f "      match t with\n";
    Terminal.iter (fun terminal ->
      if not (Terminal.pseudo terminal) then
        fprintf f "        | %s => %s%%type\n"
          (print_term terminal)
          (try print_type (Terminal.ocamltype terminal) with Not_found -> "unit")
    );
    fprintf f "      end\n";

    fprintf f "    | NT nt =>\n";
    fprintf f "      match nt with\n";
    Nonterminal.iterx (fun nonterminal ->
                         fprintf f "        | %s => %s%%type\n"
	                   (print_nterm nonterminal)
	                   (print_type (Nonterminal.ocamltype nonterminal)));
    fprintf f "      end\n";

    fprintf f "  end.\n\n"

  let write_productions f =
    write_inductive_alphabet f "production" (
      Production.foldx (fun prod l -> (print_prod prod)::l) []);
    fprintf f "Instance ProductionAlph : Alphabet production := _.\n\n"

  let write_productions_contents f =
    fprintf f "Definition prod_contents (p:production) :\n";
    fprintf f "  { p:nonterminal * list symbol &\n";
    fprintf f "    arrows (map symbol_semantic_type (snd p))\n";
    fprintf f "      (symbol_semantic_type (NT (fst p))) }\n";
    fprintf f " :=\n";
    fprintf f "  let box := existT (fun p =>\n";
    fprintf f "    arrows (map symbol_semantic_type (snd p))\n";
    fprintf f "      (symbol_semantic_type (NT (fst p))))\n";
    fprintf f "  in\n";
    fprintf f "  match p with\n";
    Production.iterx (fun prod ->
      fprintf f "    | %s => box\n" (print_prod prod);
      fprintf f "      (%s, [%s])\n"
        (print_nterm (Production.nt prod))
        (String.concat "; "
           (List.map print_symbol (Array.to_list (Production.rhs prod))));
      if Production.length prod = 0 then
        fprintf f "      (\n"
      else
        fprintf f "      (fun %s => \n"
          (String.concat " " (Array.to_list (Production.identifiers prod)));
      if Settings.coq_no_actions then
        fprintf f "()"
      else
        Action.print f (Production.action prod);
      fprintf f "\n)\n");
    fprintf f "  end.\n\n";

    fprintf f "Definition prod_lhs (p:production) :=\n";
    fprintf f "  fst (projT1 (prod_contents p)).\n";
    fprintf f "Definition prod_rhs (p:production) :=\n";
    fprintf f "  snd (projT1 (prod_contents p)).\n";
    fprintf f "Definition prod_action (p:production) :=\n";
    fprintf f "  projT2 (prod_contents p).\n\n"

  let write_nullable_first f =
    fprintf f "Definition nullable_nterm (nt:nonterminal) : bool :=\n";
    fprintf f "  match nt with\n";
    Nonterminal.iterx (fun nt ->
      fprintf f "    | %s => %b\n"
        (print_nterm nt)
        (fst (Analysis.nullable_first_rhs (Array.of_list [Symbol.N nt]) 0)));
    fprintf f "  end.\n\n";

    fprintf f "Definition first_nterm (nt:nonterminal) : list terminal :=\n";
    fprintf f "  match nt with\n";
    Nonterminal.iterx (fun nt ->
      let firstSet =
        snd (Analysis.nullable_first_rhs (Array.of_list [Symbol.N nt]) 0)
      in
      fprintf f "    | %s => [" (print_nterm nt);
      let first = ref true in
      TerminalSet.iter (fun t ->
        if !first then first := false else fprintf f "; ";
        fprintf f "%s" (print_term t)
        ) firstSet;
      fprintf f "]\n");
    fprintf f "  end.\n\n"

  let write_grammar f =
    fprintf f "Module Import Gram <: Grammar.T.\n\n";
    write_terminals f;
    write_nonterminals f;
    fprintf f "Include Grammar.Symbol.\n\n";
    write_symbol_semantic_type f;
    write_productions f;
    write_productions_contents f;
    fprintf f "Definition start_symbol := %s.\n\n" (print_symbol (Symbol.N start_nt));
    fprintf f "Include Grammar.Defs.\n\n";
    fprintf f "End Gram.\n\n"

  let write_nis f =
    write_inductive_alphabet f "noninitstate" (
      Lr1.foldx (fun l node -> (print_nis node)::l) []);
    fprintf f "Instance NonInitStateAlph : Alphabet noninitstate := _.\n\n"

  let write_actions f =
    fprintf f "Definition action_table (state:state) :\n";
    fprintf f "    default_action + (forall term:terminal, action term) :=\n";
    fprintf f "  match state with\n";
    Lr1.iter (fun node ->
      fprintf f "    | %s => " (print_st node);
      match Invariant.has_default_reduction node with
        | Some (prod, _) ->
            begin match Production.classify prod with
              | Some _ -> fprintf f "inl Accept_act\n"
              | None ->
                  fprintf f "inl (Default_reduce_act %s)\n" (print_prod prod)
            end
        | None ->
            fprintf f "inr (fun terminal:terminal =>\n";
            fprintf f "      match terminal return action terminal with\n";
            let has_fail = ref false in
            Terminal.iter (fun t ->
              if not (Terminal.pseudo t) then
                begin
                  try
                    let target = SymbolMap.find (Symbol.T t) (Lr1.transitions node) in
                    fprintf f "        | %s => Shift_act %s (eq_refl _)\n" (print_term t) (print_nis target)
                  with Not_found ->
                    try
                      let prod =
                        Misc.single (TerminalMap.find t (Lr1.reductions node))
                      in
                      fprintf f "        | %s => Reduce_act %s\n" (print_term t) (print_prod prod)
                    with Not_found -> has_fail := true
                end);
            if !has_fail then
              fprintf f "        | _ => Fail_act\n";
            fprintf f "      end)\n"
    );
    fprintf f "  end.\n\n"

  let write_gotos f =
    fprintf f "Definition goto_table (state:state) (nt:nonterminal) :=\n";
    fprintf f "  match state, nt return option { s:noninitstate | last_symb_of_non_init_state s = NT nt } with\n";
    Lr1.iter (fun node ->
      Nonterminal.iterx (fun nt ->
        try
          let target = SymbolMap.find (Symbol.N nt) (Lr1.transitions node) in
          fprintf f "    | %s, %s => Some (exist _ %s (eq_refl _))\n"
            (print_st node) (print_nterm nt) (print_nis target)
        with Not_found -> ()));
    fprintf f "    | _, _ => None\n";
    fprintf f "  end.\n\n"

  let write_last_symb f =
    fprintf f "Definition last_symb_of_non_init_state (noninitstate:noninitstate) : symbol :=\n";
    fprintf f "  match noninitstate with\n";
    Lr1.iterx (fun node ->
      match Lr1.incoming_symbol node with
        | Some s -> fprintf f "    | %s => %s\n" (print_nis node) (print_symbol s)
        | None -> assert false);
    fprintf f "  end.\n\n"

  let write_past_symb f =
    fprintf f "Definition past_symb_of_non_init_state (noninitstate:noninitstate) : list symbol :=\n";
    fprintf f "  match noninitstate with\n";
    Lr1.iterx (fun node ->
      let s =
        String.concat "; " (List.tl
          (Invariant.fold (fun l _ symb _ -> print_symbol symb::l)
             [] (Invariant.stack node)))
      in
      fprintf f "    | %s => [%s]\n" (print_nis node) s);
    fprintf f "  end.\n";
    fprintf f "Extract Constant past_symb_of_non_init_state => \"fun _ -> []\".\n\n"

  let write_past_states f =
    fprintf f "Definition past_state_of_non_init_state (s:noninitstate) : list (state -> bool) :=\n";
    fprintf f "  match s with\n";
    Lr1.iterx (fun node ->
      let s =
        String.concat ";\n        " (Invariant.fold
          (fun accu _ _ states ->
             let b = Buffer.create 16 in
             bprintf b "fun s:state =>\n";
             bprintf b "          match s return bool with\n";
             bprintf b "            ";
             Lr1.NodeSet.iter
               (fun st -> bprintf b "| %s " (print_st st)) states;
             bprintf b "=> true\n";
             bprintf b "            | _ => false\n";
             bprintf b "          end";
             Buffer.contents b::accu)
          [] (Invariant.stack node))
      in
      fprintf f "    | %s =>\n      [ %s ]\n" (print_nis node) s);
    fprintf f "  end.\n\n";
    fprintf f "Extract Constant past_state_of_non_init_state => \"fun _ -> []\".\n\n"

  let write_items f =
    if not Settings.coq_no_complete then
      begin
	Lr1.iter (fun node ->
	  fprintf f "Definition items_of_state_%d : list item :=\n" (Lr1.number node);
          fprintf f "  [ ";
          let first = ref true in
          Item.Map.iter (fun item lookaheads ->
            let prod, pos = Item.export item in
            if !first then first := false
            else fprintf f ";\n    ";
            fprintf f "{| pseudoprod_item := %s;\n" (print_pseudoprod prod);
            fprintf f "      dot_pos_item := %d;\n" pos;
            fprintf f "      lookaheads_item := [";
	    let first = ref true in
            let lookaheads =
              if TerminalSet.mem Terminal.sharp lookaheads then TerminalSet.universe
              else lookaheads
            in
	    TerminalSet.iter (fun lookahead ->
                if !first then first := false
	        else fprintf f "; ";
	        fprintf f "%s" (print_term lookahead)
	      ) lookaheads;
	    fprintf f "] |}"
          )  (Lr0.closure (Lr0.export (Lr1.state node)));
          fprintf f " ].\n";
	  fprintf f "Extract Inlined Constant items_of_state_%d => \"assert false\".\n\n" (Lr1.number node)
        );

	fprintf f "Definition items_of_state (s:state) : list item :=\n";
	fprintf f "  match s with\n";
	Lr1.iter (fun node ->
	  fprintf f "    | %s => items_of_state_%d\n" (print_st node) (Lr1.number node));
	fprintf f "  end.\n";
      end
    else
      fprintf f "Definition items_of_state (s:state): list item := [].\n";
    fprintf f "Extract Constant items_of_state => \"fun _ -> []\".\n\n"

  let write_automaton f =
    fprintf f "Module Aut <: Automaton.T.\n\n";
    fprintf f "Module Gram := Gram.\n";
    fprintf f "Module GramDefs := Gram.\n\n";
    write_nullable_first f;
    write_nis f;
    write_last_symb f;
    fprintf f "Include Automaton.Types.\n\n";
    write_actions f;
    write_gotos f;
    write_past_symb f;
    write_past_states f;
    write_items f;
    fprintf f "End Aut.\n\n"

  let write_theorems f =
    fprintf f "Require Import Main.\n\n";

    fprintf f "Module Parser := Main.Make Aut.\n";

    fprintf f "Theorem safe:\n";
    fprintf f "  Parser.safe_validator = true.\n";
    fprintf f "Proof eq_refl true<:Parser.safe_validator = true.\n\n";

    if not Settings.coq_no_complete then
      begin
        fprintf f "Theorem complete:\n";
        fprintf f "  Parser.complete_validator = true.\n";
        fprintf f "Proof eq_refl true<:Parser.complete_validator = true.\n\n";
      end;

    fprintf f "Definition parse := Parser.parse safe.\n\n";

    fprintf f "Theorem parse_correct iterator buffer:\n";
    fprintf f "  match parse iterator buffer with\n";
    fprintf f "    | Parser.Inter.Parsed_pr sem buffer_new =>\n";
    fprintf f "      exists word,\n";
    fprintf f "        buffer = Parser.Inter.app_str word buffer_new /\\\n";
    fprintf f "        Gram.has_semantic_value word sem\n";
    fprintf f "    | _ => True\n";
    fprintf f "  end.\n";
    fprintf f "Proof. apply Parser.parse_correct. Qed.\n\n";

    if not Settings.coq_no_complete then
      begin
        fprintf f "Theorem parse_complete (iterator:nat) word buffer_end (output:%s):\n"
          (print_type (Nonterminal.ocamltype start_nt));
        fprintf f "  forall tree:Gram.parse_tree (%s) word output,\n" (print_symbol (Symbol.N start_nt));
        fprintf f "  match parse iterator (Parser.Inter.app_str word buffer_end) with\n";
        fprintf f "    | Parser.Inter.Fail_pr => False\n";
        fprintf f "    | Parser.Inter.Parsed_pr output_res buffer_end_res =>\n";
        fprintf f "      output_res = output /\\ buffer_end_res = buffer_end  /\\\n";
        fprintf f "      ge iterator (Gram.parse_tree_size tree+2)\n";
        fprintf f "    | Parser.Inter.Timeout_pr => lt iterator (Gram.parse_tree_size tree+2)\n";
        fprintf f "  end.\n";
        fprintf f "Proof. apply Parser.parse_complete; exact complete. Qed.\n\n";
      end

  let write_all f =
    if not Settings.coq_no_actions then
      List.iter (fun s -> fprintf f "%s\n\n" s.Stretch.stretch_content)
        Front.grammar.UnparameterizedSyntax.preludes;

    fprintf f "Require Import List.\n";
    fprintf f "Require Import Syntax.\n";
    fprintf f "Require Import NArith.\n";
    fprintf f "Require Import Tuples.\n";
    fprintf f "Require Import Alphabet.\n";
    fprintf f "Require Grammar.\n";
    fprintf f "Require Automaton.\n\n";
    write_grammar f;
    write_automaton f;
    write_theorems f;

    if not Settings.coq_no_actions then
      List.iter (fprintf f "\n\n%s")
        Front.grammar.UnparameterizedSyntax.postludes
end

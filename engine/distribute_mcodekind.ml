open Common open Commonop

open Ast_c
module F = Control_flow_c

(*****************************************************************************)
(* When in the SP we attach something to a metavariable, or delete it, as in
 * - S
 * + foo();
 * we have to minusize all the token that compose S in the C code, and 
 * attach the 'foo();'  to the right token, the one at the very right. 
 *)
(*****************************************************************************)

type 'a distributer = 
    (Ast_c.info -> Ast_c.info) *  (* what to do on left *)
    (Ast_c.info -> Ast_c.info) *  (* what to do on middle *)
    (Ast_c.info -> Ast_c.info) *  (* what to do on right *)
    (Ast_c.info -> Ast_c.info) -> (* what to do on both *)
    'a -> 'a



(*****************************************************************************)
(* Token decoration *)
(*****************************************************************************)

type marker =
    NoMark | BefMark of string | AftMark of string
  | BefAftMark of string * string

let extract_sgrep_marker l =
  let rec inner_loop acc = function
      [] -> (acc,[])
    | Ast_cocci.SgrepStartTag(s)::rest ->
	(match acc with
	  NoMark -> inner_loop (BefMark(s)) rest
	| _ -> failwith "unexpected mark")
    | Ast_cocci.SgrepEndTag(s)::rest ->
	(match acc with
	  NoMark -> inner_loop (AftMark(s)) rest
	| BefMark(m) -> inner_loop (BefAftMark(m,s)) rest
	| _ -> failwith "unexpected mark")
    | x::rest ->
	let (acc,rest) = inner_loop acc rest in
	(acc,x::rest) in
  let (acc,l) =
    List.fold_left
      (function (acc,prev) ->
	function cur ->
	  let (acc,cur) = inner_loop acc cur in
	  (acc,cur::prev))
      (NoMark,[]) l in
  (acc,List.rev l)

let process_sgrep s2 mck =
  match mck with
    Ast_cocci.MINUS(repl) ->
      (match extract_sgrep_marker repl with
	(NoMark,_) -> mck
      |	(BefMark(marker),repl) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.file s2.line s2.column;
	  Ast_cocci.MINUS(repl)
      |	(AftMark(marker),repl) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker s2.file s2.line (s2.column + String.length s2.str);
	  Ast_cocci.MINUS(repl)
      |	(BefAftMark(bmarker,amarker),repl) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    bmarker s2.file s2.line s2.column;
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    amarker s2.file s2.line (s2.column + String.length s2.str);
	  Ast_cocci.MINUS(repl))
  | Ast_cocci.CONTEXT(Ast_cocci.NOTHING) -> mck
  | Ast_cocci.CONTEXT(Ast_cocci.BEFORE(bef)) ->
      (match extract_sgrep_marker bef with
	(NoMark,_) -> mck
      |	(BefMark(marker),[]) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.file s2.line s2.column;
	  Ast_cocci.CONTEXT(Ast_cocci.NOTHING)
      |	(BefMark(marker),bef) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.file s2.line s2.column;
	  Ast_cocci.CONTEXT(Ast_cocci.BEFORE(bef))
      |	_ -> failwith "after not possible")
  | Ast_cocci.CONTEXT(Ast_cocci.AFTER(aft)) ->
      (match extract_sgrep_marker aft with
	(NoMark,_) -> mck
      |	(AftMark(marker),[]) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker s2.file s2.line (s2.column + String.length s2.str);
	  Ast_cocci.CONTEXT(Ast_cocci.NOTHING)
      |	(AftMark(marker),aft) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker s2.file s2.line (s2.column + String.length s2.str);
	  Ast_cocci.CONTEXT(Ast_cocci.AFTER(aft))
      |	_ -> failwith "before not possible")
  | Ast_cocci.CONTEXT(Ast_cocci.BEFOREAFTER(bef,aft)) ->
      (match extract_sgrep_marker bef with
	(NoMark,_) ->
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> mck
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.file s2.line (s2.column + String.length s2.str);
	      Ast_cocci.CONTEXT(Ast_cocci.BEFORE(bef))
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.file s2.line (s2.column + String.length s2.str);
	      Ast_cocci.CONTEXT(Ast_cocci.BEFOREAFTER(bef,aft))
	  | _ -> failwith "before not possible")
      | (BefMark(marker),[]) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.file s2.line s2.column;
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> Ast_cocci.CONTEXT(Ast_cocci.AFTER(aft))
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.file s2.line (s2.column + String.length s2.str);
	      Ast_cocci.CONTEXT(Ast_cocci.NOTHING)
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.file s2.line (s2.column + String.length s2.str);
	      Ast_cocci.CONTEXT(Ast_cocci.AFTER(aft))
	  | _ -> failwith "before not possible")
      | (BefMark(marker),bef) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.file s2.line s2.column;
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> Ast_cocci.CONTEXT(Ast_cocci.BEFOREAFTER(bef,aft))
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.file s2.line (s2.column + String.length s2.str);
	      Ast_cocci.CONTEXT(Ast_cocci.BEFORE(bef))
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.file s2.line (s2.column + String.length s2.str);
	      Ast_cocci.CONTEXT(Ast_cocci.BEFOREAFTER(bef,aft))
	  | _ -> failwith "before not possible")
      |	_ -> failwith "after not possible")
  | _ -> failwith "unexpected plus code"
	
(* todo: check not already tagged ? assert s1 = s2 ? no more cos now
 * have some "fake" string, and also because now s1:'a, no more
 * s1:string *)
let tag_with_mck = fun mck ib  binding -> 
  let (s2, (oldmcode, oldenv)) = ib in
  
  let mck =
    if !Flag_parsing_cocci.sgrep_mode
    then process_sgrep s2 mck
    else mck in
  if oldmcode <> Ast_cocci.CONTEXT(Ast_cocci.NOTHING) && 
    mck <>      Ast_cocci.CONTEXT(Ast_cocci.NOTHING)
  then
    begin
      Printf.printf "SP mcode "; flush stdout;
      Pretty_print_cocci.print_mcodekind oldmcode;
      Format.print_newline();
      Printf.printf "C code mcode "; flush stdout;
      Pretty_print_cocci.print_mcodekind mck;
      Format.print_newline();
      failwith
	(Common.sprintf
	   "already tagged token:\n%s"
	   (Common.error_message s2.file (s2.str, s2.charpos)))
    end
  else 
    (s2, (mck, binding))


(*****************************************************************************)

let distribute_mck mcodekind distributef expr binding =
  match mcodekind with
  | Ast_cocci.MINUS (any_xxs) -> 
      (* could also instead add on right, it doesn't matter *)
      distributef (
        (fun ib -> tag_with_mck (Ast_cocci.MINUS (any_xxs)) ib binding),
        (fun ib -> tag_with_mck (Ast_cocci.MINUS []) ib binding),
        (fun ib -> tag_with_mck (Ast_cocci.MINUS []) ib binding),
        (fun ib -> tag_with_mck (Ast_cocci.MINUS (any_xxs)) ib binding)
      ) expr
  | Ast_cocci.CONTEXT (any_befaft) -> 
        (match any_befaft with
        | Ast_cocci.NOTHING -> expr

        | Ast_cocci.BEFORE xxs -> 
            distributef (
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.BEFORE xxs)) ib binding),
              (fun x -> x), 
              (fun x -> x), 
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.BEFORE xxs)) ib binding)
            ) expr
        | Ast_cocci.AFTER xxs ->  
            distributef (
              (fun x -> x), 
              (fun x -> x), 
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.AFTER xxs)) ib binding),
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.AFTER xxs)) ib binding)
            ) expr

        | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
            distributef (
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.BEFORE xxs)) ib binding),
              (fun x -> x), 
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.AFTER yys)) ib binding),
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (Ast_cocci.BEFOREAFTER (xxs,yys)))
                ib binding)
            ) expr

        )
  | Ast_cocci.PLUS -> raise Impossible

(*****************************************************************************)
(* Could do the minus more easily by extending visitor_c.ml and adding
 * a function applied to every mcode. But as I also need to do the
 * add_left and add_right, which requires to do a different thing for
 * each case, I have not defined this not-so-useful visitor. 
 *
 * op = minusize operator.
 * lop = stuff to do on the left.
 * rop = stuff to do on the right.
*)

(* handling list of something  separated by comma *)
let distribute_mck_split = fun funcelem (lop, mop, rop,  bop) xs -> 
  let trans_split = fun (lop, mop, rop,   bop) -> function
    | Left e -> Left (funcelem (lop, mop, rop,   bop) e)
    | Right [ii] -> 
        Right [ii +> bop]
    | Right xs -> raise Impossible
  in
  match xs with
  | [] -> raise Todo (* Impossible ? *)
  | [Left exp] -> [Left (funcelem (lop, mop, rop,   bop) exp)]
  | [Right ii] -> raise Impossible
  | x::y::xs -> 
    let (head, middle, tail) = head_middle_tail (x::y::xs) in
    [trans_split (lop, mop, mop,   lop) head]
    @ 
    List.map (trans_split (mop, mop, mop,   mop)) middle
    @
    [trans_split (mop, mop, rop,   rop) tail]
      



let rec (distribute_mck_e: Ast_c.expression distributer)= fun (lop,mop,rop,bop) e ->
  let ((unwrap_e, typ),ii) = e in
  let (e',ii') = 
    match unwrap_e, ii with
    | Ident s, [i1] -> 
        Ident s, 
        [i1 +> bop] 
    | Constant (String s),        is     -> 
        Constant (String s),
        (match is with
        | [] -> raise Impossible
        | [i] -> [i +> bop]
        | x::y::xs -> 
            let (head, middle, tail) = Common.head_middle_tail (x::y::xs) in
            [head +> lop] @ List.map mop middle @ [tail +> rop]
        )
          (* only a String can have multiple ii *)
    | Constant c,  [i1] -> 
        Constant c, 
        [i1 +> bop]

    | FunCall (e, xs), [i2;i3] -> 
        FunCall 
          (distribute_mck_e (lop, mop, mop,  lop) e,
          xs +> List.map (function 
          | (Left e, ii) -> 
              Left (distribute_mck_e (mop, mop, mop,  mop) e),
              (ii +> List.map mop)
          | (Right e, ii) -> failwith "not handling type in funcall"
          ) 
          ), 
        [i2 +> mop; i3 +> rop]

    | CondExpr (e1, e2, e3),    [i1;i2]    -> 
        CondExpr 
          (distribute_mck_e (lop, mop, mop,   lop) e1,
          Common.map_option (distribute_mck_e (mop,mop,mop,  mop)) e2,
          distribute_mck_e (mop, mop,   rop,  rop) e3),
        [i1 +> mop; i2 +> mop]
    | Sequence (e1, e2),          [i]  -> 
        Sequence
          (distribute_mck_e (lop, mop, mop,   lop) e1,
           distribute_mck_e (mop, mop, rop,   rop) e2),
        [i +> mop]
    | Assignment (e1, opbis, e2),    [i]  -> 
        Assignment
          (distribute_mck_e (lop, mop, mop,  lop) e1,
          opbis,
          distribute_mck_e (mop, mop, rop,  rop) e2),
        [i +> mop]

    | Postfix  (e, opbis),    [i] -> 
        Postfix (distribute_mck_e (lop, mop, mop,   lop) e, opbis),
        [i +> rop]
          
    | Infix    (e, opbis),    [i] -> 
        Infix (distribute_mck_e (mop, mop, rop,  rop) e, opbis),
        [i +> lop]

    | Unary    (e, opbis),    [i] -> 
        Unary (distribute_mck_e (mop, mop, rop,  rop) e, opbis),
        [i +> lop]
    | Binary   (e1, opbis, e2),    [i] -> 
        Binary
          (distribute_mck_e (lop, mop, mop,  lop) e1,
          opbis,
          distribute_mck_e (mop, mop, rop,   rop) e2),
        [i +> mop]


    | ArrayAccess    (e1, e2),   [i1;i2] -> 
        ArrayAccess
          (distribute_mck_e (lop, mop, mop,  lop) e1,
          distribute_mck_e (mop, mop, mop,  mop) e2),
        [i1 +> mop; i2 +> rop]
    | RecordAccess (e, id), [i1;i2] -> 
        RecordAccess (distribute_mck_e (lop, mop, mop,  lop) e, id), 
        [i1 +> mop; i2 +> rop]
    | RecordPtAccess (e, id), [i1;i2] -> 
        RecordPtAccess (distribute_mck_e (lop, mop, mop,  lop) e, id), 
        [i1 +> mop; i2 +> rop]

    | SizeOfExpr  (e),     [i] -> 
        SizeOfExpr (distribute_mck_e (mop, mop, rop,   rop) e),
        [i +> lop]
    | SizeOfType  (t),     [i1;i2;i3] -> 
        SizeOfType (distribute_mck_type (mop, mop, mop,  mop) t),
        [i1 +> lop; i2 +> mop; i3 +> rop]
    | Cast    (t, e),      [i1;i2] -> 
        Cast 
          (distribute_mck_type (mop, mop, mop,  mop) t,
          distribute_mck_e (mop, mop, rop,   rop) e),
        [i1 +> lop; i2 +> mop]
          
    | StatementExpr (statxs, [ii1;ii2]),  [i1;i2] -> 
        StatementExpr
          (statxs +> 
              List.map (distribute_mck_stat (mop, mop, mop,  mop)),
          [ii1 +> mop; ii2 +> mop]),
        [i1 +> lop; i2 +> rop]

    | Constructor, [] -> failwith "Constructor, what to do ? not enough info"

    | ParenExpr (e), [i1;i2] -> 
        ParenExpr (distribute_mck_e (mop, mop, mop,  mop) e),
        [i1 +> lop; i2 +> rop]

    | MacroCall  (es),     [i1;i2;i3] -> 
        failwith "MacroCall"


    | (Ident (_) | Constant _ | FunCall (_,_) | CondExpr (_,_,_) 
        | Sequence (_,_)
        | Assignment (_,_,_) 
        | Postfix (_,_) | Infix (_,_) | Unary (_,_) | Binary (_,_,_)
        | ArrayAccess (_,_) | RecordAccess (_,_) | RecordPtAccess (_,_)
        | SizeOfExpr (_) | SizeOfType (_) | Cast (_,_) 
        | StatementExpr (_) | Constructor 
        | ParenExpr (_) | MacroCall (_)
      ),_ -> raise Impossible
  in
  (e', typ), ii'


(* ------------------------------------------------------------------------- *)
and distribute_mck_arge = fun (lop,mop,rop,bop) xs -> 
  xs 
  +> Ast_c.split_comma 
  +> distribute_mck_split trans_arg (lop,mop,rop,bop)
  +> Ast_c.unsplit_comma


and trans_arg (lop,mop,rop,bop) = function
  | Left e -> Left (distribute_mck_e (lop,mop,rop,bop) e)
  | Right (ArgType (tya, (sto, iisto))) -> 
      assert (List.length iisto <= 1);
      let (iisto', lop', bop') = 
        match iisto with
        | [] -> [], lop, bop
        | [ii] -> [ii +> lop], mop, rop
        | x::xs -> raise Impossible
      in
      Right (ArgType (distribute_mck_type (lop', mop, rop, bop') tya,
                     (sto, iisto')))
        
  | Right (ArgAction action) -> 
      Right (ArgAction (trans_action (lop,mop,rop,bop) action))


and trans_action (lop,mop,rop,bop) = function
  | (ActMisc ii) -> 
      ActMisc
        (match ii with
        | [] -> []
        | [x] -> [x +> bop]
        | _ -> raise Todo (* Impossible ? *)
        )
  | (ActJump jump) -> 
      ActJump
        (match jump with
        | (Goto s), [i1;i2]               -> 
            (Goto s),
            [i1 +> lop; i2 +> rop]
        | ((Continue|Break|Return) as x), [i1] -> 
            x, 
            [i1 +> bop]
        | (ReturnExpr e), [i1] -> 
            (ReturnExpr (distribute_mck_e (mop, mop, rop,   rop) e)),
            [i1 +> lop]
        | x -> raise Impossible
        )
  | (ActSeq ((e,ii), action)) -> 
      let rop', bop' = 
        match action with 
        | ActMisc [] -> rop, bop
        | _ -> mop, lop
      in
      ActSeq (
        (match distribute_mck_stat (lop, mop, rop', bop') (ExprStatement e, ii) with
        | ExprStatement e', ii' -> e', ii'
        | _ -> raise Impossible
        ), trans_action (mop, mop, rop,   rop) action
      )



(* ------------------------------------------------------------------------- *)

and distribute_mck_params = fun (lop,mop,rop,bop) xs -> 
  xs 
  +> Ast_c.split_comma 
  +> distribute_mck_split trans_param (lop,mop,rop,bop)
  +> Ast_c.unsplit_comma

       

and trans_param (lop,mop,rop,bop) = function
  | ((b, s, t), ii_b_s) -> 
      (match b, s, ii_b_s with
      | false, Some s, [i1] -> 
          (* TODO normally could not do stuff on i1 as is
           * we should have a distribute_mck_type_with_ident func 
           *)
          (false, Some s, distribute_mck_type (lop, mop, mop,  lop) t),
          [i1 +> rop]
              
      | true, Some s, [i1;i2] -> 
          (true, Some s, 
          distribute_mck_type (mop, mop, mop,  mop) t),
          [i1 +> lop;  i2 +> rop]
            (* in definition we have name for params, except when f(void) *)
      | _, None, _ -> raise Impossible 
      | _ -> raise Impossible
      )
          




(* ------------------------------------------------------------------------- *)
and (distribute_mck_stat: Ast_c.statement distributer) = fun (lop,mop,rop,bop) -> 
  function

  | Labeled (Label (s, st)), [i1;i2] -> 
      Labeled (Label (s, distribute_mck_stat (mop, mop, rop,   rop) st)),
      [i1 +> lop;i2 +> mop]
  | Labeled (Case  (e, st)), [i1;i2] -> 
      Labeled (Case  (distribute_mck_e (mop, mop, mop,  mop) e,
                     distribute_mck_stat (mop, mop, rop,   rop) st)),
      [i1 +> lop; i2 +> mop] 
  | Labeled (CaseRange  (e, e2, st)), _ -> raise Todo
  | Labeled (Default st), [i1;i2] -> 
      Labeled (Default (distribute_mck_stat (mop, mop, rop,   rop) st)),
      [i1 +> lop; i2 +> mop]

  | Compound statxs, [i1;i2] -> 
      Compound 
        (statxs +> 
            List.map (distribute_mck_stat (mop, mop, mop,  mop))),
      [i1 +> lop; i2 +> rop]

  | ExprStatement None, [i] -> 
      ExprStatement None, 
      [i +> bop]
        (* When there is a None ? for instance with the else of a just_ifthen *)
  | ExprStatement None, [] -> 
      ExprStatement None, []
  | ExprStatement (Some e), [i] -> 
      ExprStatement (Some (distribute_mck_e (lop, mop, mop,  lop) e)),
      [i +> rop]
        (* the last ExprStatement of a for does not have a trailing ';' hence the
           [] for ii.  *)
  | ExprStatement (Some e), [] -> 
      ExprStatement (Some (distribute_mck_e (lop,mop,rop,bop) e)),
      []

  | Selection  (If (e, st1, st2)), i1::i2::i3::is -> 
      (match (st2, is) with
      | ((ExprStatement None, []), [])  -> 
          Selection 
            (If
                (distribute_mck_e (mop, mop, mop,  mop) e,
                distribute_mck_stat (mop, mop, rop,   rop) st1,
                (ExprStatement None, []))),
          [i1 +> lop; i2 +> mop; i3 +> mop]
            
            
      | st2, [i4] -> 
          Selection 
            (If
                (distribute_mck_e (mop, mop, mop,  mop) e,
                distribute_mck_stat (mop, mop, mop,  mop) st1,
                distribute_mck_stat (mop, mop, rop,   rop) st2)),
          [i1 +> lop; i2 +> mop; i3 +> mop;i4 +> mop]
            
      | x -> raise Impossible
      )

  | Selection  (Switch (e, st)), [i1;i2;i3] -> 
      Selection 
        (Switch
            (distribute_mck_e (mop, mop, mop,  mop) e,
            distribute_mck_stat (mop, mop, rop,   rop) st)),
      [i1 +> lop; i2 +> mop; i3 +> mop]
  | Iteration  (While (e, st)), [i1;i2;i3] -> 
      Iteration 
        (While
            (distribute_mck_e (mop, mop, mop,  mop) e,
            distribute_mck_stat (mop, mop, rop,   rop) st)),
      [i1 +> lop; i2 +> mop; i3 +> mop]

  | Iteration  (DoWhile (st, e)), [i1;i2;i3;i4;i5] -> 
      Iteration
        (DoWhile
            (distribute_mck_stat (mop, mop, mop,  mop) st,
            distribute_mck_e (mop, mop, mop,  mop) e)),
      [i1 +> lop; i2 +> mop; i3 +> mop; i4 +> mop; i5 +> rop]

  | Iteration  (For ((e1opt,il1),(e2opt,il2),(e3opt, il3), st)), [i1;i2;i3] ->
      assert (null il3);
      Iteration
        (For 
            ((Common.map_option 
                 (distribute_mck_e (mop, mop, mop,  mop)) e1opt, 
             il1 +> List.map mop),
            (Common.map_option 
                (distribute_mck_e (mop, mop, mop,  mop)) e2opt, 
            il2 +> List.map mop),
            (Common.map_option 
                (distribute_mck_e (mop, mop, mop,  mop)) e3opt, 
            il3 +> List.map mop),
            distribute_mck_stat (mop, mop, rop,   rop) st)),
      [i1 +> lop; i2 +> mop; i3 +> mop]
        
  | Jump (Goto s), [i1;i2;i3]               -> 
      Jump (Goto s),
      [i1 +> lop; i2 +> mop; i3 +> rop]
  | Jump ((Continue|Break|Return) as x), [i1;i2] -> 
      Jump x,
      [i1 +> lop; i2 +> rop]
  | Jump (ReturnExpr e), [i1;i2] -> 
      Jump 
        (ReturnExpr (distribute_mck_e (mop, mop, mop,  mop) e)),
      [i1 +> lop; i2 +> rop]

  | Decl decl, [] -> 
      Decl (distribute_mck_decl (mop, mop, mop,  mop) decl), []
        
  | (Asm, []) -> failwith "Asm, what to do ? not enough info"

  | Selection  (IfCpp (st1s, st2s)), i1::i2::is -> 
      raise Todo

  | ( Labeled (Label (_,_)) | Labeled (Case  (_,_)) 
      | (* Labeled (CaseRange  (_,_,_)) | *) Labeled (Default _)
      | Compound _ | ExprStatement _ 
      | Selection  (If (_, _, _)) | Selection  (Switch (_, _))
      | Iteration  (While (_, _)) | Iteration  (DoWhile (_, _)) 
      | Iteration  (For ((_,_), (_,_), (_, _), _))
      | Jump (Goto _) | Jump ((Continue|Break|Return)) | Jump (ReturnExpr _)
      | Decl _ | Asm | Selection (IfCpp (_,_))
    ), _ -> raise Impossible


(* ------------------------------------------------------------------------- *)
and (distribute_mck_decl: Ast_c.declaration distributer) = fun (lop,mop,rop,bop) ->
  fun decl ->
    (* use different strategy, collect ii, sort, recollect and tag *)
    let iidecl = Lib_parsing_c.ii_of_decl decl in
    let (maxii, minii) = Lib_parsing_c.max_min_ii_by_pos iidecl in
    let (maxpos, minpos) = 
      Ast_c.get_pos_of_info maxii, Ast_c.get_pos_of_info minii
    in
    let bigf = { 
      Visitor_c.default_visitor_c_s with
        Visitor_c.kinfo_s = (fun (k,bigf) i -> 
          let pos = Ast_c.get_pos_of_info i in
          match () with
          | _ when pos =|= maxpos && pos =|= minpos -> bop i
          | _ when pos =|= maxpos -> rop i
          | _ when pos =|= minpos -> lop i
          | _ -> mop i
        )
    } in
    Visitor_c.vk_decl_s bigf decl
      


(* ------------------------------------------------------------------------- *)
and (distribute_mck_type: Ast_c.fullType distributer) = fun (lop,mop,rop,bop) ->
  fun ((qu, iiqu),(ty, iity)) ->
    (* UGLY *)

    (* TODO in fact for pointer, the qualifier is after the type *)
    let (iiqu', lop, bop) = 
      match iiqu with
      | [] when not qu.const && not qu.volatile -> [], lop, bop
      | [i1] when xor qu.const qu.volatile -> 
          [i1 +> lop], mop, rop
      | [i1;i2] when qu.const && qu.volatile -> 
          [i1 +> lop; i2 +> mop], mop, rop
      | _ -> raise Impossible
    in
    (qu, iiqu'), 
    (match ty, iity with
    | (ParenType t, _)                           -> 
        failwith "not handling parentype"
    | (Array (eopt, t), [i1;i2])                 -> 
        failwith "not handling array"
    | (FunctionType (returnt, paramst), [i1;i2]) -> 
        failwith "not handling functiontype"
    | (Pointer (_,(Pointer _,_)), [i])                           -> 
        failwith "not handling pointer"

    (* sure that simple pointer, of if complex then catch in recursive call *)
    | (Pointer t, [i])                           -> 
        Pointer (distribute_mck_type (lop, mop, mop,  lop) t),
        [i +> rop]

    | (StructUnion (sopt, (su, fields)),iis) -> 
        failwith "not handling structunion"

    | (Enum  (sopt, enumt), iis) -> 
        failwith "not handling enum"

    | (BaseType base, iis) -> 
        BaseType base,
        (match iis with
        | [] -> raise Impossible
        | [i] -> [i +> bop]
        | x::y::xs -> 
            let (head, middle, tail) = Common.head_middle_tail (x::y::xs) in
            [head +> lop] @ List.map mop middle @ [tail +> rop]
        )
          
    | (StructUnionName (s, structunion), [i1;i2]) -> 
        StructUnionName (s, structunion), [i1 +> lop; i2 +> rop]
          
    | (EnumName  s, [i1;i2]) -> 
        EnumName s, [i1 +> lop; i2 +> rop]

    | (TypeName (s), [i1]) -> 
        TypeName s, [i1 +> bop]
          

          

    | _ -> raise Impossible
    )
      
      



(* ------------------------------------------------------------------------- *)
and (distribute_mck_node: Control_flow_c.node2 distributer) = 
  fun (lop,mop,rop,bop) -> function
  | F.Fake
  | F.Enter | F.Exit | F.ErrorExit
  | F.CaseNode _
  | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode
      -> raise Impossible
  | F.FunHeader _  -> raise Impossible

  | F.EndStatement None -> raise Impossible
  | F.EndStatement (Some i) -> 
      F.EndStatement (Some (i +> bop))
  | F.Decl decl -> F.Decl (distribute_mck_decl (lop,mop,rop,bop) decl) 

  | F.SeqStart (st, level, i1) -> 
      F.SeqStart (st, level, 
                 i1 +> bop) 
  | F.SeqEnd (level, i2) -> 
      F.SeqEnd (level, 
               i2 +> bop)


  | F.ExprStatement (st, (None, [i])) -> 
      F.ExprStatement (st, (None, 
                           [i +> bop]))
        (* when there is a None ? for instance with the else of a just_ifthen *)
  | F.ExprStatement (st, (None, [])) ->  F.ExprStatement (st, (None, []))
  | F.ExprStatement (st, (Some e, [i])) -> 
      F.ExprStatement (st, 
                      (Some (distribute_mck_e (lop, mop, mop,  lop) e),
                      [i +> rop]))
        (* the last ExprStatement of a for does not have a trailing ';' hence the
           [] for ii.  *)
  | F.ExprStatement (st, (Some e, [])) -> 
      F.ExprStatement (st, (Some (distribute_mck_e (lop,mop,rop,bop) e), []))



  | F.IfHeader (st, (e, [i1;i2;i3])) -> 
      F.IfHeader (st,
                 (distribute_mck_e (mop, mop, mop,  mop) e,
                 [i1 +> lop; i2 +> mop; i3 +> rop]))
        
  | F.Else ii -> F.Else (ii +> bop)

  | F.WhileHeader (st, (e, [i1;i2;i3])) -> 
      F.WhileHeader (st, 
                    (distribute_mck_e (mop, mop, mop,  mop) e,
                    [i1 +> lop; i2 +> mop; i3 +> rop]))

  | F.DoHeader (st, ii) -> F.DoHeader (st, ii +> bop)
  | F.DoWhileTail (e, [i1;i2;i3;i4]) -> 
      F.DoWhileTail (distribute_mck_e (mop, mop, mop,  mop) e,
                    [i1 +> lop; i2 +> mop; i3 +> mop; i4 +> rop])

  | F.ForHeader (st, (((e1opt,il1),(e2opt,il2),(e3opt, il3)), [i1;i2;i3])) -> 
      assert (null il3);
      F.ForHeader (st, 
                  (((Common.map_option 
                        (distribute_mck_e (mop, mop, mop,  mop)) e1opt, 
                    il1 +> List.map mop),
                   (Common.map_option 
                       (distribute_mck_e (mop, mop, mop,  mop)) e2opt, 
                   il2 +> List.map mop),
                   (Common.map_option 
                       (distribute_mck_e (mop, mop, mop,  mop)) e3opt, 
                   il3 +> List.map mop)),
                  [i1 +> lop; i2 +> mop; i3 +> rop]))


  | F.SwitchHeader (st, (e, [i1;i2;i3])) -> 
      F.SwitchHeader (st, 
                     (distribute_mck_e (mop, mop, mop,  mop) e,
                     [i1 +> lop; i2 +> mop; i3 +> rop]))

  | F.Return   (st, ((), [i1;i2])) -> 
      F.Return (st, ((), 
                    [i1 +> lop; i2 +> rop]))
  | F.ReturnExpr (st, (e, [i1;i2])) -> 
      F.ReturnExpr (st, 
                   (distribute_mck_e (mop, mop, mop,  mop) e,
                   [i1 +> lop; i2 +> rop]))

  (* ------------------------ *)
  (* no counter part in cocci *)
  | F.Label (st, (s, [i1;i2])) -> 
      F.Label (st, (s, 
                   [i1 +> lop; i2 +> rop]))
  | F.Case  (st, (e, [i1;i2])) -> 
      F.Case (st, 
             (distribute_mck_e (mop, mop, mop,  mop) e,
             [i1 +> lop; i2 +> rop]))
  | F.CaseRange (st,  ((e1, e2), ii)) -> raise Todo
  | F.Default (st, ((), [i1; i2])) -> 
      F.Default (st, ((), 
                     [i1 +> lop; i2 +> rop]))

  | F.Goto (st, (s, [i1;i2;i3])) -> 
      F.Goto (st, (s, 
                  [i1 +> lop; i2 +> mop; i3 +> rop]))
  | F.Continue (st, ((), [i1;i2])) -> 
      F.Continue (st, ((),
                      [i1 +> lop; i2 +> rop]))

  | F.Break   (st, ((), [i1;i2])) -> 
      F.Break (st, ((),
                   [i1 +> lop; i2 +> rop]))

  | F.Asm -> F.Asm

  | F.IfCpp (_,_) -> raise Todo

  | F.CPPDefine _ -> raise Todo
  | F.CPPInclude _ -> raise Todo

  | ( F.ExprStatement (_, _) 
      | F.IfHeader  (_, _) | F.SwitchHeader (_, _)
      | F.WhileHeader (_, _) | (* F.DoHeader (_, _) | *) F.DoWhileTail (_, _) 
      | F.ForHeader (_, _)
      | F.Return     (_, _)  | F.ReturnExpr (_, _)
            (* no counter part in cocci *)
      | F.Label (_, _) 
      | F.Case  (_,_) | (* F.CaseRange (_, _) | *) F.Default   (_, _)
      | F.Goto (_, _) | F.Continue (_, _) | F.Break    (_, _)
    ) -> raise Impossible


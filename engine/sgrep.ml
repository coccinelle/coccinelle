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
    Ast_cocci.MINUS(pos,repl) ->
      (match extract_sgrep_marker repl with
	(NoMark,_) -> mck
      |	(BefMark(marker),repl) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line s2.Common.column;
	  Ast_cocci.MINUS(pos,repl)
      |	(AftMark(marker),repl) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line
	    (s2.Common.column + String.length s2.Common.str);
	  Ast_cocci.MINUS(pos,repl)
      |	(BefAftMark(bmarker,amarker),repl) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    bmarker s2.Common.file s2.Common.line s2.Common.column;
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    amarker s2.Common.file s2.Common.line
	    (s2.Common.column + String.length s2.Common.str);
	  Ast_cocci.MINUS(pos,repl))
  | Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING) -> mck
  | Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef)) ->
      (match extract_sgrep_marker bef with
	(NoMark,_) -> mck
      |	(BefMark(marker),[]) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line s2.Common.column;
	  Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING)
      |	(BefMark(marker),bef) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line s2.Common.column;
	  Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef))
      |	_ -> failwith "after not possible")
  | Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft)) ->
      (match extract_sgrep_marker aft with
	(NoMark,_) -> mck
      |	(AftMark(marker),[]) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line
	    (s2.Common.column + String.length s2.Common.str);
	  Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING)
      |	(AftMark(marker),aft) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line
	    (s2.Common.column + String.length s2.Common.str);
	  Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft))
      |	_ -> failwith "before not possible")
  | Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft)) ->
      (match extract_sgrep_marker bef with
	(NoMark,_) ->
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> mck
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.Common.file s2.Common.line
		(s2.Common.column + String.length s2.Common.str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef))
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.Common.file s2.Common.line
		(s2.Common.column + String.length s2.Common.str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft))
	  | _ -> failwith "before not possible")
      | (BefMark(marker),[]) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line s2.Common.column;
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft))
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.Common.file s2.Common.line
		(s2.Common.column + String.length s2.Common.str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING)
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.Common.file s2.Common.line
		(s2.Common.column + String.length s2.Common.str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft))
	  | _ -> failwith "before not possible")
      | (BefMark(marker),bef) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker s2.Common.file s2.Common.line s2.Common.column;
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft))
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.Common.file s2.Common.line
		(s2.Common.column + String.length s2.Common.str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef))
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker s2.Common.file s2.Common.line
		(s2.Common.column + String.length s2.Common.str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft))
	  | _ -> failwith "before not possible")
      |	_ -> failwith "after not possible")
  | _ -> failwith "unexpected plus code"

module PC = Parser_c

type position = Before | After | Middle

let make_int str info = PC.TInt((str,(Ast_c.Signed,Ast_c.CInt)),info)
let make_float str info = PC.TFloat((str,(Ast_c.CFloat)),info)
let make_quote str isW _ info = PC.TQuote ((str,isW),info)
let make_pct str info = PC.TPct info
let make_format str info = PC.TFormat (str,info)
let make_sub_string str info = PC.TSubString (str,info)

let update_info str offset oldinfo pos maker =
  let pinfo =
    match oldinfo.Ast_c.pinfo with
      Ast_c.OriginTok oldpinfo ->
        Ast_c.OriginTok
          {oldpinfo with
	    Common.str = str;
            Common.charpos = oldpinfo.Common.charpos + offset;
            Common.column = oldpinfo.Common.column + offset}
    | Ast_c.FakeTok _ -> failwith "fake"
    | Ast_c.ExpandedTok(pi,vp) ->
	failwith
	  (Printf.sprintf "expanded: %s" (Dumper.dump oldinfo.Ast_c.pinfo))
    | Ast_c.AbstractLineTok(pi) -> failwith "abstract"
    (*|  _ -> failwith "bad parse info token"*) in
  let newinfo =
    match pos with
      Middle ->
	{Ast_c.pinfo = pinfo;
	  Ast_c.cocci_tag = ref Ast_c.emptyAnnot;
	  annots_tag = Token_annot.empty;
	  comments_tag = ref Ast_c.emptyComments;
	  danger = ref Ast_c.NoDanger;
	}
    | Before ->
	let ct = !(oldinfo.Ast_c.comments_tag) in
	let ct = {ct with
		   Ast_c.mafter = [];
		   Ast_c.mafter2 = [];
		 } in
	{Ast_c.pinfo = pinfo;
	  Ast_c.cocci_tag = ref Ast_c.emptyAnnot;
	  annots_tag = Token_annot.empty;
	  comments_tag = ref ct;
	  danger = ref Ast_c.NoDanger;
	}
    | After ->
	let ct = !(oldinfo.Ast_c.comments_tag) in
	let ct = {ct with
		   Ast_c.mbefore = [];
		   Ast_c.mbefore2 = [];
		 } in
	{Ast_c.pinfo = pinfo;
	  Ast_c.cocci_tag = ref Ast_c.emptyAnnot;
	  annots_tag = Token_annot.empty;
	  comments_tag = ref ct;
	  danger = ref Ast_c.NoDanger;
	} in
  maker str newinfo

let pct_split str =
  let lst = Common.list_of_string str in
  let complete l =
    let l = List.rev l in
    String.concat "" (List.map (function c -> Printf.sprintf "%c" c) l) in
  let rec loop acc cur = function
      [] -> List.rev ((complete cur)::acc)
    | '%'::'%'::rest -> loop acc ('%'::'%'::cur) rest
    | ['%'] -> raise Parse_printf.Not_format_string
    | '%'::rest -> loop ((complete cur)::acc) [] rest
    | x :: rest -> loop acc (x :: cur) rest in
  loop [] [] lst

let parse_middle middle info offset =
  let pieces = pct_split middle in
  let string_to_frag str info offset =
    update_info str offset info Middle make_sub_string in
  match pieces with
    [] | [_] -> raise Parse_printf.Not_format_string
  | fst::rest ->
      let (first,offset) =
        if fst = ""
        then ([],offset)
        else ([(string_to_frag fst info offset)],offset + String.length fst) in
      let (rest,_) =
        List.fold_left
          (function (prev,offset) ->
	    function r ->
              let pct = update_info "%" offset info Middle make_pct in
	      let offset = offset + 1 in
	      let after_offset = offset + String.length r in
              let mkfmt d offset =
		update_info d offset info Middle make_format in
	      let (c1,rest) = Parse_printf.get_format_string r in
              let first = [mkfmt c1 offset;pct] in
              if rest = ""
              then (first@prev,after_offset)
              else
		let rest_offset = offset + String.length c1 in
		((string_to_frag rest info rest_offset)::first@prev,
		 after_offset))
          (first,offset) rest in
      rest

let not_format_string (str,isW) info = [PC.TString((str,isW),info)]

let parse_string (str,isW) info =
  if List.length(Str.split_delim (Str.regexp "%") str) > 1
  then
    try
      begin
	let first = update_info "\"" 0 info Before (make_quote str isW) in
	let last =
	  update_info "\"" (String.length str+1) info After
	    (make_quote str isW) in
	let middle = parse_middle str info 1 in
	List.rev (last :: middle @ [first])
      end
    with Parse_printf.Not_format_string -> not_format_string (str,isW) info
  else not_format_string (str,isW) info

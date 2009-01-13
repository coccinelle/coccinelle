exception FileNotFound of string

let match_f bug fe =
  let (fb, _, _, _) = bug in
  let (fe, _, _) = fe in
    fb = fe

let get_femin fel bug =
  let (fb, _, _, _) = bug in
  try
    let (_, fmin, _) = List.find (fun fe -> match_f bug fe) fel in
      fmin
  with _ -> raise (FileNotFound fb)

let get_femax fel bug =
  let (fb, _, _, _) = bug in
  try
    let (_, _, fmax) = List.find (fun fe -> match_f bug fe) fel in
      fmax
  with _ -> raise (FileNotFound fb)

let compute_bug_info fel bug =
  let fmin = get_femin fel bug in
  let fmax = get_femax fel bug in
  let (fb,c,bmin,bmax) = bug in
    (fb, c, fmin, bmin, bmax, fmax)

let compute_graph fel fbl =
    List.map (compute_bug_info fel) fbl

let cd_ratio max_ver bugs =
  List.fold_left
    (fun (i,j) (fc, c, fmin, bmin, bmax, fmax) ->
       ((if (fmin = bmin && fmin != 0) then i+1 else i),
	(if (fmax = bmax && fmax != max_ver) then j+1 else j))
    ) (0,0) bugs

let sl_ratio bugs =
  List.fold_left
    (fun (i,j) (fc, c, fmin, bmin, bmax, fmax) ->
       let duration = bmax - bmin + 1 in
       ((if (duration < i) then duration else i),
	(if (duration > j) then duration else j))
    ) (max_int, 0) bugs

let verbose_stat bugs =
  List.iter (fun (fc, c, fmin, bmin, bmax, fmax) ->
	       prerr_endline (fc ^ " - " ^ c ^
				" fm: " ^ (string_of_int fmin) ^
				" bm: " ^ (string_of_int bmin) ^
				" bM: " ^ (string_of_int bmax) ^
				" fM: " ^ (string_of_int fmax)))
    bugs

let show_stat verbose max_ver fel fbl =
  let fn = List.length fel in
  let bn = List.length fbl in
  let bugs = compute_graph fel fbl in
  let (create, delete) = cd_ratio max_ver bugs in
  let (shortest, longest) = sl_ratio bugs in
    (if verbose then
       verbose_stat bugs; prerr_newline ()
    );
    prerr_string ((string_of_int fn)^ " file(s) affected by ");
    prerr_endline ((string_of_int bn)^ " bug(s)");
    prerr_float (float_of_int bn/. float_of_int fn);
    prerr_endline " bug(s) per file";
    prerr_int create;
    prerr_string " bug(s) (";
    prerr_int (create * 100 / bn);
    prerr_endline "%) introduced by a new file. ";
    prerr_int delete;
    prerr_string " bug(s) (";
    prerr_int (delete * 100 / bn);
    prerr_endline "%) removed by a file deletion.";
    prerr_int shortest;
    prerr_endline " version(s) for the shortest bug life.";
    prerr_int longest;
    prerr_endline " version(s) for the longest bug life."

let draw_graph max fel fbl =
  let size = List.length fbl in
  Printf.printf "
newgraph
xaxis min 0 max %02d size 7 hash 1 mhash 0 label : kernel versions
draw_at -1
yaxis min 0 max %02d size 5 label : bugs
no_draw_hash_marks no_draw_hash_labels

newcurve marktype ybar color 0 0 0
pts\n" (max+1) (size - 1);
  for i = 0 to size - 1 do
    Printf.printf " %02d %02d\n" (max+1) i;
  done;
  Printf.printf "
newcurve marktype ybar color 1 1 1
pts\n";
  ignore(List.fold_left (fun i bug ->
			   let femax = get_femax fel bug in
			   let (_,_,bmax,_) = bug in
			     (if (femax > bmax) then
			       Printf.printf " %02d %02d\n" (femax+1) i;
			     );
			     i+1) 0 fbl);
  Printf.printf "
newcurve marktype ybar color 1 0 0
pts";
  ignore(List.fold_left (fun i (_,_,_, bmax) ->
		    Printf.printf " %02d %02d\n" (bmax+1) i;
		    i+1) 0 fbl);
  Printf.printf "
newcurve marktype ybar color 1 1 1
pts";
  ignore(List.fold_left (fun i (_,_,bmin,_) ->
		    Printf.printf " %02d %02d\n" bmin i;
		    i+1) 0 fbl);
  Printf.printf "
newcurve marktype ybar color 0 0 0
pts\n";
  ignore(List.fold_left (fun i bug ->
			   let m = get_femin fel bug in
			     (if (m > 0) then
				Printf.printf " %02d %02d\n" m i
			     );
			     i+1) 0 fbl)

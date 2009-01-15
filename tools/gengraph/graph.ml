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

let update_dir dir assoc =
  try
    let v = List.assoc dir assoc in
    let res = List.remove_assoc dir assoc in
      (dir, v+1)::res
  with Not_found ->
    (dir, 1)::assoc

let compute_bydir bugs =
  let p1_re = Str.regexp "^\\([^/]+\\)/.*$" in
    List.fold_left (fun res (fb, _, _, _) ->
		      if(Str.string_match p1_re fb 0) then
			let dir = Str.matched_group 1 fb in
			  update_dir dir res
		      else
			update_dir "" res
		   ) [] bugs

let cd_ratio max_ver bugs =
  List.fold_left
    (fun (i,j) (fc, c, fmin, bmin, bmax, fmax) ->
       ((if (fmin = bmin && fmin != 0) then i+1 else i),
	(if (fmax = bmax && fmax != max_ver) then j+1 else j))
    ) (0,0) bugs

let sl_ratio bugs =
  let (shortest, sum, longest) =  List.fold_left
    (fun (i,j,k) (fc, c, fmin, bmin, bmax, fmax) ->
       let duration = bmax - bmin + 1 in
       ((if (duration < i) then duration else i),
	j + duration,
	(if (duration > k) then duration else k))
    ) (max_int, 0, 0) bugs
  in
  let mean = float_of_int sum /. float_of_int (List.length bugs) in
    (shortest, mean, longest)

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
  let (shortest, mean, longest) = sl_ratio bugs in
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
    prerr_endline " version(s) for the longest bug life.";
    prerr_float mean;
    prerr_endline " version(s) for the mean bug life.";
    List.iter (fun (dir, v) ->
		 prerr_int v;
		 if ((String.compare dir "") == 0) then
		   prerr_endline "in root directory"
		 else
		   (prerr_string " in dir ";
		    prerr_endline dir
		   )
	      ) (compute_bydir fbl)

let draw_graph max fel fbl =
  let size = List.length fbl in
  Printf.printf "
newgraph
xaxis min 0 max %02d size 5 hash 1 mhash 0 label : kernel versions
draw_at -1
yaxis min 0 max %02d size 7 label : bugs
no_draw_hash_marks no_draw_hash_labels no_draw_axis

newcurve marktype ybar color 0 0 0
pts\n" (max+1) (size - 1);
  ignore(List.fold_left (fun i bug ->
			   let femax = get_femax fel bug in
			     (if (femax < max) then
			       Printf.printf " %02d %02d\n" (max+1) i;
			     );
			     i+1) 0 fbl);
  Printf.printf "
newcurve marktype ybar color 1 1 1
pts\n";
  ignore(List.fold_left (fun i bug ->
			   let femax = get_femax fel bug in
			   let (_,_,_,bmax) = bug in
			     (if (femax > bmax) then
			       Printf.printf " %02d %02d\n" (femax+1) i
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
  ignore(List.fold_left (fun i bug ->
			   let femin = get_femin fel bug in
			   let (_,_,bmin,_) = bug in
			     (if (femin < bmin) then
				Printf.printf " %02d %02d\n" bmin i
			     );
			      i+1) 0 fbl);
  Printf.printf "
newcurve marktype ybar color 0 0 0
pts\n";
  ignore(List.fold_left (fun i bug ->
			   let femin = get_femin fel bug in
			     (if (femin > 0) then
				Printf.printf " %02d %02d\n" femin i
			     );
			     i+1) 0 fbl);
  Printf.printf "
newstring
hjl vjc
(* fonsize 9 *)
font Helvetica-Narrow\n";
  let (f0, _, _,_) = List.hd fbl in
  Printf.printf " x %02d y 00 : %s\n" (max+2) f0;
  ignore(List.fold_left (fun i (f, _, _,_) ->
			   Printf.printf " copystring y %02d : %s\n"  i f;
			   i+1) 1 (List.tl fbl))


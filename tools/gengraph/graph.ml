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

let show_stat fel fbl =
  List.iter (fun (fc, c, fmin, bmin, bmax, fmax) ->
	       prerr_endline (fc ^ " - " ^ c ^
				" fm: " ^ (string_of_int fmin) ^
				" bm: " ^ (string_of_int bmin) ^
				" bM: " ^ (string_of_int bmax) ^
				" fM: " ^ (string_of_int fmax)))
    (compute_graph fel fbl)

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

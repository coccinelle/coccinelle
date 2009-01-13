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

let draw_graph max fel fbl =
  let size = List.length fbl in
  Printf.printf "
newgraph
xaxis min 0 max %02d size 2 label : kernel versions
yaxis min 0 max %02d size 5 label : bugs
no_draw_hash_marks no_draw_hash_labels

newcurve marktype ybar color 0 0 0
pts\n" max (size - 1);
  for i = 0 to size - 1 do
    Printf.printf " %02d %02d\n" max i;
  done;
  Printf.printf "
newcurve marktype ybar color 1 1 1
pts\n";
  ignore(List.fold_left (fun i bug ->
			   let femax = get_femax fel bug in
			   let (_,_,bmax,_) = bug in
			     (if (femax > bmax) then
			       Printf.printf " %02d %02d\n" femax i;
			     );
			     i+1) 0 fbl);
  Printf.printf "
newcurve marktype ybar color 1 0 0
pts";
  ignore(List.fold_left (fun i (_,_,_, bmax) ->
		    Printf.printf " %02d %02d\n" bmax i;
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

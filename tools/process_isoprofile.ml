(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./process_isoprofile.ml"
(* This is for processing information created with the -profile_iso option.
Runs are assumed separated with a line beginning with =.
The first run is discarded *)

let is_marker l  = String.get l 0 = '='
let is_nothing l = String.sub l 0 2 = "ls"

let skip_start i = (* skip over the ========== at the beginning *)
  let rec loop _ =
    let l = input_line i in
    if not (is_marker l)
    then loop() in
  loop()

let get_data l =
  match Str.split (Str.regexp ":") l with
    [_;after] ->
      (match Str.split (Str.regexp " sec") after with
	[info;_] -> float_of_string info
      |	_ -> failwith "bad data")
  | _ -> failwith (Printf.sprintf "bad data %s" l)

type more = MORE | NOMORE | INFO of float * float * float * float | CONT

let read_data_one i =
  try
    let start = input_line i in (* three lines of header *)
    if is_marker start
    then MORE
    else if is_nothing start
    then CONT
    else
      let _ = input_line i in
      let _ = input_line i in
      (match
	List.sort compare
	  [input_line i;input_line i;input_line i;input_line i]
      with
	[asttoctl;full_engine;mysat;parse_cocci] ->
	  if String.get full_engine 0 = '*'
	  then (let _ = input_line i in CONT) (* hack!!! *)
	  else
	    let asttoctl = get_data asttoctl in
	    let full_engine = get_data full_engine in
	    let mysat = get_data mysat in
	    let parse_cocci = get_data parse_cocci in
	    INFO(full_engine,mysat,parse_cocci,asttoctl)
      |	_ -> failwith "not possible")
  with End_of_file -> NOMORE

let read_data i =
  skip_start i;
  let optcons x y = if x = [] then y else x::y in
  let rec loop all_acc acc =
    match read_data_one i with
      NOMORE -> optcons acc all_acc
    | MORE -> loop (optcons acc all_acc) []
    | CONT -> loop all_acc acc
    | INFO(a,b,c,d) -> loop all_acc ((a,b,c,d)::acc) in
  let table = loop [] [] in
  let all_infos = (* a list with a list of information for each file *)
    List.fold_left
      (function all_infos ->
	function one_run ->
	  List.map2 (function ainfo -> function orun -> orun::ainfo)
	    all_infos one_run)
      (List.map (function _ -> []) (List.hd table))
      table in
  let overheads =
    List.concat
      (List.map (List.map (function (_,x,y,z) -> x+.y+.z)) all_infos) in
  let total_times =
    List.concat
      (List.map (List.map (function (x,_,_,_) -> x)) all_infos) in
  let mysat_times =
    List.concat
      (List.map (List.map (function (_,x,_,_) -> x)) all_infos) in
  let parse_time =
    List.concat
      (List.map (List.map (function (_,_,x,y) -> x +. y)) all_infos) in
  (overheads,total_times,mysat_times,parse_time)

let percent pct = (int_of_float ((100.0 *. pct) +. 0.5)) - 100
let mpercent pct = (int_of_float ((100.0 *. pct) +. 0.5))
let minf l = List.fold_left min (List.hd l) l
let maxf l = List.fold_left max (List.hd l) l

let ave = function
    [] -> 0.0
  | l ->
      let total = List.fold_left (+.) 0.0 l in
      total /. (float_of_int(List.length l))

let process_files iso_file noiso_file =
  let i = open_in iso_file in
  let (iso_over,iso_total,iso_mysat,iso_parse) = read_data i in
  close_in i;
  let i = open_in noiso_file in
  let (noiso_over,noiso_total,noiso_mysat,noiso_parse) = read_data i in
  close_in i;
  Printf.printf "isos:   min %f max %f ave %f\n"
    (minf iso_total) (maxf iso_total) (ave iso_total);
  Printf.printf "noisos: min %f max %f ave %f\n"
    (minf noiso_total) (maxf noiso_total) (ave noiso_total);
  Printf.printf "Overhead in total time %d%%: min %f max %f\n"
    (percent (ave (List.map2 (/.) iso_total noiso_total)))
    (minf (List.map2 (-.) iso_total noiso_total))
    (maxf (List.map2 (-.) iso_total noiso_total));
  Printf.printf "Portion of overhead due to parsing %d%%: min %f max %f\n"
    (mpercent
       (ave (List.fold_left2
	       (function acc ->
		 (function (iso_total,iso_parse) ->
		   (function (noiso_total,noiso_parse) ->
		     let total_ovd = iso_total -. noiso_total in
		     let parse_ovd = iso_parse -. noiso_parse in
		     if total_ovd < 0.001 or parse_ovd > total_ovd or
		       parse_ovd < 0.0
		     then acc
		     else (parse_ovd /. total_ovd) :: acc)))
	       []
	       (List.combine iso_total iso_parse)
	       (List.combine noiso_total noiso_parse))))
    (minf (List.map2 (-.) iso_parse noiso_parse))
    (maxf (List.map2 (-.) iso_parse noiso_parse));
  Printf.printf "Portion of overhead due to matching %d%%: min %f max %f\n\n"
    (mpercent
       (ave (List.fold_left2
	       (function acc ->
		 (function (iso_total,iso_mysat) ->
		   (function (noiso_total,noiso_mysat) ->
		     let total_ovd = iso_total -. noiso_total in
		     let mysat_ovd = iso_mysat -. noiso_mysat in
		     if total_ovd < 0.001 or mysat_ovd > total_ovd or
		       mysat_ovd < 0.0
		     then acc
		     else (mysat_ovd /. total_ovd) :: acc)))
	       []
	       (List.combine iso_total iso_mysat)
	       (List.combine noiso_total noiso_mysat))))
    (minf (List.map2 (-.) iso_mysat noiso_mysat))
    (maxf (List.map2 (-.) iso_mysat noiso_mysat))

let _ =
  let iso = Array.get Sys.argv 1 in
  let noiso = Array.get Sys.argv 2 in
  process_files iso noiso

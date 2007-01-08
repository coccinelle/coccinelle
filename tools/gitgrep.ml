type dir = Minus | Plus

let space = Str.regexp " "

let scan pattern dir i =
  let rec loop skipping git =
    let line = input_line i in
    match Str.split space line with
      ["commit";git] -> loop false git
    | "-"::_ when not skipping && dir = Minus ->
	(try
	  let _ = Str.search_forward pattern line 0 in
	  Printf.printf "%s\n" git;
	  loop true git
	with Not_found -> loop skipping git)
    | "+"::_ when not skipping && dir = Plus ->
	(try
	  let _ = Str.search_forward pattern line 0 in
	  Printf.printf "%s\n" git;
	  loop true git
	with Not_found -> loop skipping git)
    | _ -> loop skipping git in
  loop false ""


let _ =
  if not(Array.length Sys.argv = 4)
  then failwith "arguments: -/+ text file";
  let dir =
    match Array.get Sys.argv 1 with
      "-" -> Minus
    | "+" -> Plus
    | _ -> failwith "bad direction" in
  let pattern = Str.regexp (Array.get Sys.argv 2) in
  let i = open_in (Array.get Sys.argv 3) in
  try scan pattern dir i
  with End_of_file -> close_in i

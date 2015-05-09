exception Failed

let interpret dir query =
  try
    List.fold_left
      (function prev ->
	function cur ->
	  let cmd = Printf.sprintf "cd %s; git grep -l -w %s" cur in
	  let (res,code) = Common.cmd_to_list_and_status cmd in
	  if code = 0
	  then Common.inter_set res prev
	  else raise Failed)
      [] query
  with Failed -> []

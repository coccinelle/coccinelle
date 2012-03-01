exception Git_cmd of string

let exec_with handle_line str =
  let chan = Unix.open_process_in str in
  let _ = try
    while true do
      let line = input_line chan in
      handle_line line
    done
    with End_of_file -> () in
  let status = Unix.close_process_in chan in
  match status with
    Unix.WEXITED 0 -> ()
  | _              -> raise (Git_cmd str)

let clone repo local =
  let tmpDir = Filename.temp_file "rev" "scan" in
  Sys.remove tmpDir;
  let localParam = if local then "--local " else "" in
  let cmd = "git clone --no-checkout " ^ localParam ^ repo ^ " " ^ tmpDir in
  exec_with print_endline cmd;
  tmpDir

let reset () =
  let cmd = "git reset -q --hard" in
  exec_with print_endline cmd

let checkout rev =
  let cmd = "git checkout --detach -fq " ^ rev in
  exec_with print_endline cmd

let intermediate_revs fromRev toRev =
  let revs = Queue.create () in
  let cmd = "git log --no-color --no-merges --date-order --format=%H " ^ fromRev ^ ".." ^ toRev  in
  exec_with (function str -> Queue.push str revs) cmd;
  revs

let rev_info rev =
  ()

(* git diff --no-color --name-only --diff-filter=M *)

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

let intermediate_revs fromRev toRev =
  let revs = Queue.create () in
  let cmd = "git log --no-color --no-merges --date-order --format=%H " ^ fromRev ^ ".." ^ toRev  in
  exec_with (function str -> Queue.push str revs) cmd;
  revs

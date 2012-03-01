let clone repo local =
  let tmpDir = Command.mk_temp_dir () in
  let localParam = if local then "--local " else "" in
  let cmd = "git clone --no-checkout " ^ localParam ^ repo ^ " " ^ tmpDir in
  Command.exec_with print_endline cmd;
  tmpDir

let reset () =
  let cmd = "git reset -q --hard" in
  Command.exec_with print_endline cmd

let checkout rev =
  let cmd = "git checkout --detach -fq " ^ rev in
  Command.exec_with print_endline cmd

let intermediate_revs fromRev toRev =
  let revs = Queue.create () in
  let cmd = "git log --no-color --no-merges --date-order --format=%H " ^ fromRev ^ ".." ^ toRev  in
  Command.exec_with (function str -> Queue.push str revs) cmd;
  if Queue.length revs == 0 then raise (Command.Cmd_failed ("no revisions by: " ^ cmd));
  revs

let rev_info rev =
  ()

(* git diff --no-color --name-only --diff-filter=M *)

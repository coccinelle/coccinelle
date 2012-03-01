(*
  Analyzer wrapper on git C repositories.
*)

let processBase base =
  (* collect removed lines *)
  (* convert to changed 'blocks' *)
  Command.exec_hook Options.opts.Options.hookBase

let processRev rev =
  (* collect added lines *)
  (* convert to changed 'blocks' *)
  Command.exec_hook Options.opts.Options.hookRev

let processChangeset base rev =
  Git.checkout base;
  Command.exec_hook Options.opts.Options.hookBegin;
  processBase base;
  Git.checkout rev;
  processRev rev;

  Command.exec_hook Options.opts.Options.hookAnalyze;
  Command.exec_hook Options.opts.Options.hookFinish;
  Git.reset ()

let setup () =
  let cdir = Sys.getcwd () in
  at_exit (function () -> Sys.chdir cdir);
  let target = !(Options.opts.Options.target) in
  let wdir =
        match target with
          Options.Temp -> Git.clone !(Options.opts.Options.repo) !(Options.opts.Options.local)
        | Options.Exists dir -> dir in
  Sys.chdir wdir;
  match target with
    Options.Temp     -> ()
  | Options.Exists _ -> Git.reset ()

let main () =
  Options.initialize Sys.argv;
  setup ();
  let revs = Git.intermediate_revs !(Options.opts.Options.fromRev) !(Options.opts.Options.toRev) in
  Command.exec_hook Options.opts.Options.hookSetup;
  let prev = ref (Queue.pop revs) in
  let process rev =
        let base = !prev in
        processChangeset base rev;
        prev := rev in
  Queue.iter process revs;
  Command.exec_hook Options.opts.Options.hookTearDown

let prefix_app_name str =
  Sys.argv.(0) ^ ": " ^ str

let catch_errors f =
  let errMsg = ref None in
  let _ = try f () with
      Arg.Bad msg -> errMsg := Some msg
    | Command.Cmd_failed cmd -> errMsg := Some ("command failed: " ^ cmd)
  in !errMsg

let _ = match catch_errors main with
        None     -> exit 0
      | Some msg -> 
          prerr_endline msg;
          exit 1

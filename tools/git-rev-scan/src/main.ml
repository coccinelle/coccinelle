(*
  Analyzer wrapper on git C repositories.
*)

let processRev rev =
  Git.checkout rev;

  (* do something of interest here *)
  print_endline rev;
  Git.reset ()

let setup opts =
  let cdir = Sys.getcwd () in
  match opts.Options.target with
    Options.Temp ->
      let dir = Git.clone opts.Options.repo opts.Options.local in
      at_exit (function () -> Sys.chdir cdir; let _ = Unix.system ("rm -rf " ^ dir) in ());
      Sys.chdir dir
  | Options.Exists dir ->
      at_exit (function () -> Sys.chdir cdir);
      Sys.chdir dir;
      Git.reset ()

let main () =
  Options.initialize Sys.argv;
  let opts = Options.get_opts () in
  setup opts;
  let revs = Git.intermediate_revs opts.Options.fromRev opts.Options.toRev in
  Queue.iter processRev revs

let prefix_app_name str =
  Sys.argv.(0) ^ ": " ^ str

let catch_errors f =
  let errMsg = ref None in
  let _ = try f () with
      Arg.Bad msg     -> errMsg := Some msg
    | Git.Git_cmd cmd -> errMsg := Some ("command failed: " ^ cmd)
  in !errMsg

let _ = match catch_errors main with
        None     -> exit 0
      | Some msg -> 
          prerr_endline msg;
          exit 1

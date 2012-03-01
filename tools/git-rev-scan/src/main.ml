(*
  Analyzer wrapper on git C repositories.
*)

let main () =
  Options.initialize Sys.argv;
  let opts = Options.get_opts () in
  Sys.chdir opts.Options.repo;
  let revs = Git.intermediate_revs opts.Options.fromRev opts.Options.toRev in
  Queue.iter print_endline revs
 
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

type options = {
  repo : string;
  fromRev : string;
  toRev : string;
  script : string;
}

let init = {
  repo = ".";
  fromRev = "";
  toRev = "";
  script = ".";
}

let opts = ref init

let get_opts () = !opts

let fail_anon_arg str =
  raise (Arg.Bad ("unexpected anonymous argument: " ^ str ))

let mk_args_spec repoRef fromRef toRef scriptRef = [
    ("--repo", Arg.Set_string repoRef, "path to the git repository");
  ("--from", Arg.Set_string fromRef, "starting revision");
  ("--to", Arg.Set_string toRef, "ending revision");
  ("--script", Arg.Set_string scriptRef, "script to execute");
]

let mk_usage_str argv = "Usage: " ^ argv.(0) ^ " --repo <path>"

let get_settings argv =
  let repoRef = ref init.repo in
  let fromRef = ref init.fromRev in
  let toRef = ref init.toRev in
  let scriptRef = ref init.script in
  let usageStr = mk_usage_str argv in
  let argsSpec = mk_args_spec repoRef fromRef toRef scriptRef in
  let current = ref 0 in
  Arg.parse_argv ~current argv argsSpec fail_anon_arg usageStr;
  { repo = !repoRef; fromRev = !fromRef; toRev = !toRef; script = !scriptRef }

let check_settings opts =
  if (not (Sys.file_exists opts.repo)) || (not (Sys.is_directory opts.repo))
  then raise (Arg.Bad ("not a directory: " ^ opts.repo));
  
  if (not (Sys.file_exists opts.script)) || Sys.is_directory opts.script
  then raise (Arg.Bad ("not a script: " ^ opts.script));

  if String.length opts.fromRev == 0
  then raise (Arg.Bad ("no 'from' revision given."));

  if String.length opts.toRev == 0
  then raise (Arg.Bad ("no 'to' revision given."));
  ()

let initialize argv =
  let newOpts = get_settings argv in
  check_settings newOpts;
  opts := newOpts

type target = Temp | Exists of string

type options = {
  repo         : string ref;
  fromRev      : string ref;
  toRev        : string ref;
  local        : bool ref;
  target       : target ref;

  hookSetup    : (string option) ref;
  hookBegin    : (string option) ref;
  hookBase     : (string option) ref;
  hookRev      : (string option) ref;
  hookAnalyze  : (string option) ref;
  hookFinish   : (string option) ref;
  hookTearDown : (string option) ref;
}

let opts = {
  repo         = ref ".";
  fromRev      = ref "";
  toRev        = ref "";
  local        = ref false;
  target       = ref Temp;
  
  hookSetup    = ref None;
  hookBegin    = ref None;
  hookBase     = ref None;
  hookRev      = ref None;
  hookAnalyze  = ref None;
  hookFinish   = ref None;
  hookTearDown = ref None;
}

let fail_anon_arg str =
  raise (Arg.Bad ("unexpected anonymous argument: " ^ str))

let is_dir path = Sys.file_exists path && Sys.is_directory path
let is_script path = Sys.file_exists path && not (Sys.is_directory path)

let set_target targetRef str =
  if String.length str == 0
  then targetRef := Temp
  else if not (is_dir str)
       then raise (Arg.Bad ("not a directory: " ^ str));
       targetRef := Exists str

let set_hook hookRef str =
  if String.length str == 0
  then hookRef := None
  else if not (is_script str)
       then raise (Arg.Bad ("not a script: " ^ str));
       hookRef := Some str

let args_spec = [
  ("--repo",         Arg.Set_string opts.repo, "path to the git repository");
  ("--from",         Arg.Set_string opts.fromRev, "starting revision");
  ("--to",           Arg.Set_string opts.toRev, "ending revision");
  ("--local",        Arg.Set opts.local, "use a local clone");
  ("--target",       Arg.String (set_target opts.target),     "path to existing repo (omit for temp)");
  ("--hook-setup",   Arg.String (set_hook opts.hookSetup),    "setup/initialize hook");
  ("--hook-begin",   Arg.String (set_hook opts.hookBegin),    "begin hook");
  ("--hook-base",    Arg.String (set_hook opts.hookBase),     "old rev hook");
  ("--hook-rev",     Arg.String (set_hook opts.hookRev),      "new rev hook");
  ("--hook-analyze", Arg.String (set_hook opts.hookAnalyze),  "analyze hook");
  ("--hook-finish",  Arg.String (set_hook opts.hookFinish),   "finishup hook");
  ("--hook-tear",    Arg.String (set_hook opts.hookTearDown), "tear down hook");
]

let mk_usage_str argv = "Usage: " ^ argv.(0) ^ " --repo <path>"

let initialize argv =
  let usageStr = mk_usage_str argv in
  let current = ref 0 in
  Arg.parse_argv ~current argv args_spec fail_anon_arg usageStr;

  if not (is_dir !(opts.repo))
  then raise (Arg.Bad ("repository does not point to a directory: " ^ !(opts.repo)));

  if String.length !(opts.fromRev) == 0
  then raise (Arg.Bad ("no 'from' revision given."));

  if String.length !(opts.toRev) == 0
  then raise (Arg.Bad ("no 'to' revision given."));
  ()

external proc_pidpath: int -> string = "proc_pidpath_wrapper"

let get_executable_path () =
  proc_pidpath (Unix.getpid ())

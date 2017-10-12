external proc_pidpath: int -> string = "proc_pidpath_wrapper"
(** On Mac OS, returns the executable path of the given PID.
    Returns the empty string on other operating systems. *)

val get_executable_path: unit -> string
(** Returns the executable path of the current process. *)

val realpath: string -> string
(** Expands symbolic links in a path. *)

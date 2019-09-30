val argv : string array
val executable_name : string
external file_exists : string -> bool = "caml_sys_file_exists"
external is_directory : string -> bool = "caml_sys_is_directory"
external remove : string -> unit = "caml_sys_remove"
external rename : string -> string -> unit = "caml_sys_rename"
external getenv : string -> string = "caml_sys_getenv"
val getenv_opt : string -> string option
external command : string -> int = "caml_sys_system_command"
external time :
  unit -> ((float)[@unboxed ]) = "caml_sys_time" "caml_sys_time_unboxed"
[@@noalloc ]
external chdir : string -> unit = "caml_sys_chdir"
external getcwd : unit -> string = "caml_sys_getcwd"
external readdir : string -> string array = "caml_sys_read_directory"
val interactive : bool ref
val os_type : string
type backend_type =
  | Native 
  | Bytecode 
  | Other of string 
val backend_type : backend_type
val unix : bool
val win32 : bool
val cygwin : bool
val word_size : int
val int_size : int
val big_endian : bool
val max_string_length : int
val max_array_length : int
external runtime_variant : unit -> string = "caml_runtime_variant"
external runtime_parameters : unit -> string = "caml_runtime_parameters"
type signal_behavior =
  | Signal_default 
  | Signal_ignore 
  | Signal_handle of (int -> unit) 
external signal :
  int -> signal_behavior -> signal_behavior = "caml_install_signal_handler"
val set_signal : int -> signal_behavior -> unit
val sigabrt : int
val sigalrm : int
val sigfpe : int
val sighup : int
val sigill : int
val sigint : int
val sigkill : int
val sigpipe : int
val sigquit : int
val sigsegv : int
val sigterm : int
val sigusr1 : int
val sigusr2 : int
val sigchld : int
val sigcont : int
val sigstop : int
val sigtstp : int
val sigttin : int
val sigttou : int
val sigvtalrm : int
val sigprof : int
val sigbus : int
val sigpoll : int
val sigsys : int
val sigtrap : int
val sigurg : int
val sigxcpu : int
val sigxfsz : int
exception Break 
val catch_break : bool -> unit
val ocaml_version : string
val enable_runtime_warnings : bool -> unit
val runtime_warnings_enabled : unit -> bool
external opaque_identity : 'a -> 'a = "%opaque"

let library_patterns: (int -> int -> string, unit, string) format list =
  ["libpython%d.%dm.dylib"; "libpython%d.%d.dylib"]

let library_suffix = ".dylib"

let ensure_executable_suffix executable = executable

let which = "which"

external fd_of_int: int -> Unix.file_descr = "%identity"

let path_separator = ":"

let library_patterns: (int -> int -> string, unit, string) format list =
  ["python%d%d.dll"]

let library_suffix = ".dll"

let ensure_executable_suffix executable =
  if Filename.check_suffix executable ".exe" then
    executable
  else
    executable ^ ".exe"

let which = "where"

external fd_of_int: int -> Unix.file_descr = "win_handle_fd"

let path_separator = ";"

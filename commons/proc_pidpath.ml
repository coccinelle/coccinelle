external proc_pidpath: int -> string = "proc_pidpath_wrapper"

external realpath: string -> string = "realpath_wrapper"

let get_executable_path () =
  match proc_pidpath (Unix.getpid ()) with
  | "" ->
     begin
       match
         try Some (Unix.readlink "/proc/self/exe")
         with _ -> None
       with
         Some executable -> executable
       | None ->
          match
            try Some (Sys.getenv "_")
            with Not_found -> None
          with
            Some executable -> executable
          | None -> Sys.argv.(0)
     end
  | path -> path

open Common open Commonop

let dir = ref false

let cocci_file = ref ""
let iso_file = ref ""

(******************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let options = Arg.align
      [ 
        "-dir",                Arg.Set        dir, "   process all files in directory recursively";
        "-cocci_file",         Arg.Set_string cocci_file, "   the semantic patch file";
        "-iso_file",           Arg.Set_string iso_file, "   the iso file";

      ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <path-to-c-dir>\nOptions are:") in
    Arg.parse options (fun file -> args := file::!args) usage_msg;

    (match (!args) with

    | x::xs -> 
        if (!cocci_file = "") then failwith "I need a cocci file,  use -cocci_file <filename>";
        if not (!cocci_file =~ ".*\\.cocci") then pr2 "warning: seems not a .cocci file";
        if !iso_file <> "" && not (!iso_file =~ ".*\\.iso") then pr2 "warning: seems not a .iso file";

        let fullxs = 
          if !dir 
          then (assert (xs = []); process_output_to_list ("find " ^ x ^" -name \"*.c\"")) 
          else x::xs 
        in

        fullxs +> List.iter (fun cfile -> 
          Cocci.full_engine cfile (Left (!cocci_file, (if !iso_file = "" then None else Some !iso_file) ))
            );

    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
   );
   (* pr2 (profiling_diagnostic ()); *)
   end


let _ = if not (!Sys.interactive) then main ()

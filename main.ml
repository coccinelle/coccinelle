open Common open Commonop

let dir = ref false

let cocci_file = ref ""
let iso_file = ref ""

let test_mode = ref false

(******************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let options = Arg.align
      [ 
        "-dir",                Arg.Set        dir, "   process all files in directory recursively";
        "-cocci_file",         Arg.Set_string cocci_file, "   the semantic patch file";
        "-iso_file",           Arg.Set_string iso_file, "   the iso file";

        "-test",                Arg.Set        test_mode, "  automatically find the corresponding c and cocci file";
        "-show_ctl",            Arg.Set        Flag.show_ctl, " ";
        "-show_flow",           Arg.Set        Flag.show_flow, " ";

      ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <path-to-c-dir>\nOptions are:") in
    Arg.parse options (fun file -> args := file::!args) usage_msg;

    (match (!args) with

    | [x] when !test_mode -> 
        let cfile = "tests/" ^ x ^ ".c" in 
        let cocci_file =  "tests/" ^ x ^ ".cocci" in
        let iso = (Some "standard.iso") in
        Cocci.full_engine cfile (Left (cocci_file, iso))
        

    | x::xs -> 

        if (!cocci_file = "") then failwith "I need a cocci file,  use -cocci_file <filename>";
        if not (!cocci_file =~ ".*\\.cocci") then pr2 "warning: seems not a .cocci file";
        if !iso_file <> "" && not (!iso_file =~ ".*\\.iso") then pr2 "warning: seems not a .iso file";
        let cocci_file = !cocci_file in
        let iso_file = (if !iso_file = "" then None else Some !iso_file) in

        let fullxs = 
          if !dir 
          then (assert (xs = []); process_output_to_list ("find " ^ x ^" -name \"*.c\"")) 
          else x::xs 
        in

        fullxs +> List.iter (fun cfile -> 
          Cocci.full_engine cfile (Left (cocci_file, iso_file))
            );

    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
   );
   (* pr2 (profiling_diagnostic ()); *)
   end


let _ = if not (!Sys.interactive) then main ()

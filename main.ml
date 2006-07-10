open Common open Commonop

let dir = ref false

let cocci_file = ref ""
let iso_file = ref ""

let test_mode = ref false
let test_ctl_foo = ref false

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

        "-verbose_ctl_engine",   Arg.Set       Flag_ctl.verbose_ctl_engine, " ";

        "-test_ctl_foo",         Arg.Set       test_ctl_foo, "  test the engine with the foo ctl in test.ml";


      ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <path-to-c-dir>\nOptions are:") in
    Arg.parse options (fun file -> args := file::!args) usage_msg;

    (match (!args) with

    | [x] when !test_ctl_foo -> 
        let cfile = x in 
        Cocci.full_engine cfile (Right (Test.foo_ctl ()));
        

    | [x] when !test_mode -> 
        let base = 
          if x =~ "\\(.*\\)_ver[0-9]+"
          then matched1 x
          else x
        in
        let x' = if x =~ "\\(.*\\)_ver0" then matched1 x else x in
        let cfile = "tests/" ^ x' ^ ".c" in 
        let cocci_file =  "tests/" ^ base ^ ".cocci" in
        if !iso_file <> "" && not (!iso_file =~ ".*\\.iso")
	then pr2 "warning: seems not a .iso file";
        let iso_file =
	  if !iso_file = "" then Some "standard.iso" else Some !iso_file in
        Cocci.full_engine cfile (Left (cocci_file, iso_file));
        let expected_res = "tests/" ^ x ^ ".res" in
        if Common.lfile_exists expected_res 
        then 
          let xs = process_output_to_list ("diff -b -B " ^ "/tmp/output.c" ^ " "  ^ expected_res) in
          if null xs 
          then pr2 ("seems correct (comparing to " ^ expected_res ^ ")")
          else 
            begin
              pr2 "seems incorrect";
              pr2 "diff (result(<) vs expected_result(>)) = ";
              xs +> List.iter pr2;
            end
            
          
        

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

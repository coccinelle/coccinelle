open Fullcommon open Commonop

(* todo:
    in fact fragile, cf txt/diff_format.txt,  
    "^--- " is not enough for a regexp,  there must be just before a diff command
   todo?: does it handle when a file was just created/deleted ? 
    --- a/drivers/acorn/char/defkeymap-acorn.c_shipped	Sat Jun 14 12:18:56 2003
    +++ /dev/null	Wed Dec 31 16:00:00 1969
   does it need to ? (I think no, after all, if deleted or created, there is no
    interesting patch info to print)

*)

type patchinfo = (filename, fileinfo) oassoc
     and fileinfo = ((int * int) * string) list
     (* inv: the regions are sorted, because we process patch from start to end of file *)

let (parse_patch: (string list) -> patchinfo) = fun lines ->
  let lines =  lines        +> List.filter (fun s -> not (s =~ "^\\+\\+\\+" || s =~ "^diff"))    in

  (* note: split_list_regexp can generate __noheading__ category (cf common.ml code) *)
  let double_splitted = 
    lines 
      +> split_list_regexp "^\\-\\-\\- "  
      +> List.map (fun (s, group) -> 
          (s, split_list_regexp "^@@" group) 
         )
  in

  double_splitted +>         
     List.map (fun (s, group) -> 
       let _ = (s =~ "^\\-\\-\\- [^/]+/\\([^/]+\\)/\\(.*\\)/\\([^ \t]+\\)[ \t]") 
            +> (fun b -> if not b then (pr2 s; pr2 (Dumper.dump group); assert b)) in
       let (driver_or_sound, subdirs, filename) = matched3 s in
        (* assert drivers|sound     *)
          (driver_or_sound ^ "/" ^ subdirs ^ "/" ^ filename, 

           group +>
             List.map (fun (s, group) -> 
               let _ = (s =~ "^@@ \\-\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@") 
                   +> (fun b -> if not b then pr2 s; assert b) in
               let (start1, plus1, _start2, _plus2) = matched4 s in
               let (start1, plus1) = pair s_to_i (start1, plus1) in
               ((start1, start1 + plus1), 
                 (* just put the +- into the string ? *)
                  unlines group
               )
             )
          )
       )
     +> (new oassocb [])#fromlist 

  
  
  
let (relevant_part: (filename * (int * int)) -> patchinfo -> string) = fun (filename, (startl, endl)) patchinfo ->
  try 
    let xs = patchinfo#find filename in
    let is_in i (min, max) = i >= min && i <= max in
    xs +> map_filter (fun ((i,j), s) -> 
      if ((is_in i (startl, endl)) || (is_in j (startl, endl)) || (i < startl && j > endl))
      then Some s 
      else None
      ) 
      +> String.concat "\nOTHER REGION\n" 

  with Not_found -> ("NO MODIF in patch file for:" ^ filename)





(*******************************************************************************)

let (filter_driver_sound: string list -> string list) = fun lines -> 
  let res = ref [] in

  let state = ref 0 in

  begin
  lines +> List.iter (fun s -> 
    if s =~ "^\\-\\-\\- .*" then state := 0;

    (* GET ALSO Documentation,  include/*(except asm-  ?   futur=fs, net *)
    (* could filter .h too,  newfile *)
    if s =~ "^\\-\\-\\- [^/]+/\\([^/]+\\)/\\(.*\\)/\\([a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_]+\\)[ \t]"
    then
       let (driver_or_sound, subdirs, filename) = matched3 s in
       if driver_or_sound = "drivers" || driver_or_sound = "sound"
       then begin state := 1; push2 s res end
       else state := 0
    else 
      if s =~ "^\\-\\-\\- .*" then state := 0
      else 
        if !state = 1 then push2 s res
        else ()
            );
   List.rev !res
  end



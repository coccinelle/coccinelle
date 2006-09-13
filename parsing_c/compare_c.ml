open Common open Commonop

open Ast_c


type compare_result = 
  | Correct 
  | Incorrect of string
  | IncorrectOnlyInNotParsedCorrectly

let compare (c1,stat1) (c2, stat2)  =

    let c1' = Abstract_line_c.al_program (c1 +> List.map fst) in
    let c2' = Abstract_line_c.al_program (c2 +> List.map fst) in
    
    let xs =
      process_output_to_list ("diff -u -b -B " ^ 
                              stat1.Parse_c.filename ^ " "  ^ 
                              stat2.Parse_c.filename) 
    in
    let error = ref 0 in
    let pb_notparsed = ref 0 in
    
    let res = 
     if List.length c1' <> List.length c2' 
     then Incorrect "not same number of entities (func, decl, ...)"
     else 
       begin
         zip c1' c2' +> List.iter (function
           | Declaration a, Declaration b -> if not (a =*= b) then incr error
           | Definition a, Definition b ->   if not (a =*= b) then incr error
           | EmptyDef a, EmptyDef b ->       if not (a =*= b) then incr error
           | SpecialDeclMacro (a1,b1,c1), SpecialDeclMacro (a2,b2,c2) -> 
               if not ((a1,b1,c1) =*= (a2,b2,c2)) then incr error
           | CPPInclude a, CPPInclude b -> if not (a =*= b) then incr error
           | CPPDefine a, CPPDefine b ->   if not (a =*= b) then incr error
           | NotParsedCorrectly a, NotParsedCorrectly b -> 
               if not (a =*= b) then incr pb_notparsed
           | NotParsedCorrectly a, _ -> 
               incr pb_notparsed
           | _, NotParsedCorrectly b -> 
               incr pb_notparsed
           | FinalDef a, FinalDef b -> if not (a =*= b) then incr error
           | _, _ -> incr error
                        );
         (match () with
         | _ when !pb_notparsed > 0 && !error = 0 -> 
             IncorrectOnlyInNotParsedCorrectly
         | _ when !error > 0 -> Incorrect ""
         | _ -> Correct
         )
       end
    in
    res, xs

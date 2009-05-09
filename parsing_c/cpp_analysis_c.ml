(* Yoann Padioleau
 * 
 * Copyright (C) 2009 University of Urbana Champaign
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common 

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

(* callgraph of macros *)

type key = string
type node = (Common.filename * Cpp_token_c.define_def) list ref
type edge = Direct

type callgraph_macros = (key, node, edge) Ograph_simple.ograph_mutable

  
(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(*****************************************************************************)
(* Builder  *)
(*****************************************************************************)

let build_callgraph_macros xs = 
  raise Todo

(*****************************************************************************)
(*   *)
(*****************************************************************************)


let dangerous_macros  xs = 
  let all_macros = 
    xs +> List.map (fun (file, defs) -> 
      defs +> List.map (fun def -> file, def)
    ) +> List.flatten
  in
  
  let all_macros' = 
    all_macros +> List.filter (fun (file, (x, def)) -> 
      let (s, params, body) = def in 

      assert(s =$= x);
      (match params, body with
      | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [Parser_c.TInt _]
      | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [] -> 
          false
      | _ -> 
          let bodytoks = 
            match body with
            | Cpp_token_c.DefineHint _ -> 
                pr2 "weird, hint in cpp_analysis_c";
                []
            | Cpp_token_c.DefineBody xs -> 
                xs
          in
          true
    )
    )
  in
  let grouped = Common.group_assoc_bykey_eff all_macros' in
  grouped
                              
(*

      );

            | () when s ==~ Parsing_hacks.regexp_annot -> true
            | () when List.exists (function
                 (*| Parser_c.Tattribute _ -> true*)
                 | Parser_c.TCppConcatOp _ -> true
                 | _ -> false) bodytoks 
               -> true
            | () -> false
          in
*)

open Common open Commonop

(* When in a semantic patch there is
     f(X)
     ...
     f(X)
 we want to force the two X to be equal in the concrete code, but we would like 
 that there be equal modulo some isomorphisms, so that the following concrete 
 code also match:
   f(a && b)
   g();
   f(b && a)

*)

open Ast_c

let rec (iso_e_e: expression -> expression -> bool) = fun a b -> 
  let rec (=~=) a b = 
  match (a, b) with
  | (Ident a, typa, iia), (Ident b, typb, iib) -> a = b
  | (Constant a, typa, iia), (Constant b, typb, iib) -> a = b
  | (FunCall  (ea, eas), typa, iia), (FunCall  (eb, ebs), typb, iib)        -> 
      ea =~= eb &&
      List.length eas = List.length ebs && 
      List.for_all (fun (a, b) -> 
        match (a, b) with
        | (Left ea, iia), (Left eb, iib) -> ea =~= eb
        | _ -> raise Todo
        )
        (zip eas ebs)
  | (Binary (ea1,Logical AndLog,ea2),typa, iia), (Binary (eb1,Logical AndLog, eb2), typb, iib) -> 
      (ea1 =~= eb1  && ea2 =~= eb2) 
       ||
      (ea1 =~= eb2  && ea2 =~= eb1)

  | _ -> raise Todo
  in
  a =~= b

and (iso_st_st: statement -> statement -> bool) = fun a b -> 
  raise Todo
and (iso_t_t: fullType -> fullType -> bool) = fun a b -> 
  raise Todo

  


let ex1 = 
           (Binary
             ((Ident "a",
               None,
               [({str = "a"; charpos = 19},
                 (Ast_cocci.CONTEXT
                   ({Ast_cocci.line = -1; Ast_cocci.logical_line = -1},
                   {contents = Ast_cocci.NOTHING}),
                  []))]),
             Logical AndLog,
             (Ident "b",
              None,
              [({str = "b"; charpos = 22},
                (Ast_cocci.CONTEXT
                  ({Ast_cocci.line = -1; Ast_cocci.logical_line = -1},
                  {contents = Ast_cocci.NOTHING}),
                 []))])),
            None,
            [({str = "&&"; charpos = 20},
              (Ast_cocci.CONTEXT
                ({Ast_cocci.line = -1; Ast_cocci.logical_line = -1},
                {contents = Ast_cocci.NOTHING}),
               []))])

let ex2 = 
           (Binary
             ((Ident "b",
              None,
               [({str = "b"; charpos = 28},
                 (Ast_cocci.CONTEXT
                   ({Ast_cocci.line = -1; Ast_cocci.logical_line = -1},
                   {contents = Ast_cocci.NOTHING}),
                  []))]),
             Logical AndLog,
             (Ident "a",
              None,
              [({str = "a"; charpos = 31},
                (Ast_cocci.CONTEXT
                  ({Ast_cocci.line = -1; Ast_cocci.logical_line = -1},
                  {contents = Ast_cocci.NOTHING}),
                 []))])),
            None,
            [({str = "&&"; charpos = 29},
              (Ast_cocci.CONTEXT
                ({Ast_cocci.line = -1; Ast_cocci.logical_line = -1},
                {contents = Ast_cocci.NOTHING}),
               []))])

let _ = assert (iso_e_e ex1 ex2)



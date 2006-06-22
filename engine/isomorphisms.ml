open Common open Commonop

module A = Ast_cocci


let mmvide =
  A.CONTEXT ({A.line = -1; A.logical_line = -1; A.offset = -1}, ref A.NOTHING)
(******************************************************************************)

let rec isomorphisms_re = function
  | A.ExprStatement (expression ,string_mcode) -> 
      A.ExprStatement (isomorphisms_e expression, string_mcode)
  | x -> x


and isomorphisms_e = function
  | A.Unary (e, (A.Not, m)) -> 
      A.DisjExpr [
      A.Unary (e, (A.Not, m));
      A.Binary (e, (A.Logical A.Eq, mmvide), A.Ident (A.Id ("NULL", mmvide)))
    ] 
        
  | x -> x



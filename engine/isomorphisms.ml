open Common open Commonop

module A = Ast_cocci


let mmvide = A.CONTEXT(A.NOTHING)
let infovide = {A.line = -1; A.column = -1}
(******************************************************************************)

let rec isomorphisms_re = function
  | A.ExprStatement (expression ,string_mcode) -> 
      A.ExprStatement (isomorphisms_e expression, string_mcode)
  | x -> x


and isomorphisms_e = function
  | A.Unary (e, (A.Not, i, m)) -> 
      A.DisjExpr [
      A.Unary (e, (A.Not, i, m));
      A.Binary (e, (A.Logical A.Eq, infovide, mmvide),
		A.Ident (A.Id ("NULL", infovide, mmvide)))
    ] 
        
  | x -> x



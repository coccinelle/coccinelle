open Common open Commonop

open Ast_c

let rec eq_type a b = 
  let ((qua,iiqa), (tya,iia)) = a in
  let ((qub,iiqb), (tyb,iib)) = b in
  qua.const =:= qub.const && qua.volatile =:= qub.volatile &&
  

  match tya, tyb with
  | BaseType a, BaseType b -> a =*= b
  | Pointer a, Pointer b -> eq_type a b
  | Array (ea, a), Array (eb,b) -> 
      (* dont care about size ? *)
      eq_type a b

  | FunctionType fta, FunctionType ftb -> 
      raise Todo

  | Enum (saopt, enuma), Enum (sbopt, enumb) -> raise Todo
  | StructUnion (sua, saopt, sta), StructUnion (sub, sbopt, stb) -> raise Todo

  | EnumName sa, EnumName sb -> sa =$= sb
  | StructUnionName (sua, sa), StructUnionName (sub, sb) -> 
      sua =*= sub && sa =$= sb
  | TypeName (sa, opta), TypeName (sb, optb) -> 
      (* assert compatible opta optb ? *)
      sa =$= sb
  | ParenType a, ParenType b -> 
      (* iso here ? *)
         eq_type a b

  | TypeOfType ea, TypeOfExpr eb -> 
      raise Todo
  | TypeOfType a, TypeOfType b -> 
      eq_type a b

(*  | TypeOfType a, b -> 
    | a, TypeOfType b -> 
*)
  (* typedef iso *)
  | TypeName (_, Some a), tyb -> 
      eq_type a b(*topleve b*)
  | tya, TypeName (_, Some b) -> 
      eq_type a(*toplevel a*) b
  | _, _ -> false

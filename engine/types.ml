open Common  open Commonop

module A = Type_cocci
module B = Ast_c

let rec compatible_type a b = 
  match a, b with
  | A.BaseType (a, signa), (qua, (B.BaseType b,ii)) -> 
      (match a, b with
      | A.VoidType, B.Void -> 
          assert (signa = None);
          true
      | A.CharType, B.IntType B.CChar when signa = None -> true
      | A.CharType, B.IntType (B.Si (signb, B.CChar2)) -> 
          compatible_sign signa signb 
      | A.ShortType, B.IntType (B.Si (signb, B.CShort)) -> 
          compatible_sign signa signb
      | A.IntType, B.IntType (B.Si (signb, B.CInt)) -> 
          compatible_sign signa signb
      | A.LongType, B.IntType (B.Si (signb, B.CLong)) -> 
          compatible_sign signa signb
      | _, B.IntType (B.Si (signb, B.CLongLong)) -> 
          pr2 "no longlong in cocci";
          false
      | A.FloatType, B.FloatType B.CFloat -> assert (signa = None); 
          true
      | A.DoubleType, B.FloatType B.CDouble -> assert (signa = None); 
          true
      | _, B.FloatType B.CLongDouble -> 
          pr2 "no longdouble in cocci";
          false
      | A.BoolType, _ -> failwith "no booltype in C"
      | _ -> false
  
      )
  | A.Pointer  a, (qub, (B.Pointer b, ii)) -> 
      compatible_type a b
  | A.Array   a, (qub, (B.Array (eopt, b),ii)) -> (* no size info for cocci *)
      compatible_type a b
  | A.StructUnionName (sua, sa), (qub, (B.StructUnionName (sb, sub),ii)) -> 
      equal_structUnion sua sub && sa = sb
  | A.TypeName sa, (qub, (B.TypeName sb, ii)) -> 
      sa = sb

  | A.ConstVol (qua, a), (qub, b) -> 
      (match qua with 
      | A.Const -> (fst qub).B.const
      | A.Volatile -> (fst qub).B.volatile
      ) && 
      compatible_type a (qub, b)

  | A.MetaType        s, _ -> 
      failwith "todo?: so this function should return a binding ?"
  (* for metavariables of type expression *^* *)
  | A.Unknown , _ -> true

  | _ -> false

and compatible_sign signa signb = 
  match signa, signb with
  | None, B.Signed -> true
  | Some A.Signed, B.Signed -> true
  | Some A.Unsigned, B.UnSigned -> true
  | _ -> false

and equal_structUnion a b = 
  match a, b with
  | A.Struct, B.Struct -> true
  | A.Union,  B.Union -> true
  | _, _ -> false

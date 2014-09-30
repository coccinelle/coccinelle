module Ast = Ast_cocci

(* ------------------------------------------------------------------------- *)
(* TOSTRING FUNCTIONS FOR AST_COCCI BASE TYPES *)

(* takes a list of 'a and concatenates it using fn ('a -> string),
 * delimitering with between (string) *)
let between_tostring between tostring_fn =
  let rec between_tostring' acc between fn = function
    | [] -> acc
    | [x] -> acc ^ (fn x)
    | x::xs -> between_tostring' (acc ^ (fn x) ^ between) between fn xs in
  between_tostring' "" between tostring_fn

let meta_tostring (r, x) = x

let constant_tostring = function
  | Ast.String(s) -> "\"" ^ s ^ "\""
  | Ast.Char(s) -> "'" ^ s ^ "'"
  | Ast.Int(s) -> s
  | Ast.Float(s) -> s
  | Ast.DecimalConst(s,_,_) -> s

let struct_union_tostring = function
  | Ast.Struct -> "struct"
  | Ast.Union -> "union"

let sign_tostring = function
  | Ast.Signed -> "signed"
  | Ast.Unsigned -> "unsigned"

let const_vol_tostring = function
  | Ast.Const -> "const"
  | Ast.Volatile -> "volatile"

let storage_tostring = function
  | Ast.Static -> "static"
  | Ast.Auto -> "auto"
  | Ast.Register -> "register"
  | Ast.Extern -> "extern"

let inc_elem_tostring = function
  | Ast.IncPath s -> s
  | Ast.IncDots -> "..."

let inc_file_tostring = function
  | Ast.Local(elems) ->
      "\"" ^ (between_tostring "/" inc_elem_tostring elems) ^ "\""
  | Ast.NonLocal(elems) ->
      "<" ^ (between_tostring "/" inc_elem_tostring elems) ^ ">"

let fix_tostring = function
  | Ast.Dec -> "--"
  | Ast.Inc -> "++"

let arith_tostring = function
  | Ast.Plus -> "+"
  | Ast.Minus -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Min -> "<?"
  | Ast.Max -> ">?"
  | Ast.Mod -> "%"
  | Ast.DecLeft -> "<<"
  | Ast.DecRight -> ">>"
  | Ast.And -> "&"
  | Ast.Or -> "|"
  | Ast.Xor -> "^"

let logic_tostring = function
  | Ast.Inf -> "<"
  | Ast.Sup -> ">"
  | Ast.InfEq -> "<="
  | Ast.SupEq -> ">="
  | Ast.Eq -> "=="
  | Ast.NotEq -> "!="
  | Ast.AndLog -> "&&"
  | Ast.OrLog -> "||"

let unary_tostring = function
  | Ast.GetRef -> "&"
  | Ast.GetRefLabel -> "&&"
  | Ast.DeRef -> "*"
  | Ast.UnPlus -> "+"
  | Ast.UnMinus -> "-"
  | Ast.Tilde -> "~"
  | Ast.Not -> "!"

let binary_tostring = function
  | Ast.Arith(aop) -> arith_tostring aop
  | Ast.Logical(lop) -> logic_tostring lop

let assign_tostring = function
  | Ast.SimpleAssign -> "="
  | Ast.OpAssign(aop) -> arith_tostring aop ^ "="

let type_tostring = function
  | Ast.VoidType -> "void"
  | Ast.CharType -> "char"
  | Ast.ShortType -> "short"
  | Ast.ShortIntType -> "short int"
  | Ast.IntType -> "int"
  | Ast.DoubleType -> "double"
  | Ast.LongDoubleType -> "long double"
  | Ast.FloatType -> "float"
  | Ast.LongType -> "long"
  | Ast.LongIntType -> "long int"
  | Ast.LongLongType -> "long long"
  | Ast.LongLongIntType -> "long long int"
  | Ast.SizeType -> "size_t"
  | Ast.SSizeType -> "ssize_t"
  | Ast.PtrDiffType -> "ptrdiff_t"

let whenmodifier_tostring = function
  | Ast.WhenAny -> "any"
  | Ast.WhenStrict -> "strict"
  | Ast.WhenForall -> "forall"
  | Ast.WhenExists -> "exists"

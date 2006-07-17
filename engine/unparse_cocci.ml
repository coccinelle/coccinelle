open Common open Commonop

module Ast = Ast_cocci

let term ((s,_,_) : 'a Ast_cocci.mcode) = s

(* or perhaps can have in plus, for instance a Disj, but
   those Disj must be handled by interactive tool (by proposing alternatives) *)
exception CantBeInPlus

(* --------------------------------------------------------------------- *)


let rec pp_list_list_any (env, current_tabbing, pr, pr_elem) xxs =

(* Just to be able to copy paste the code from unparse_cocci.ml. *)
let print_string = pr 
in
let close_box() = () 
in
let print_space() = pr " " 
in
let force_newline () = ()
in

let start_block () = ()
in
let end_block () = ()
in
let print_string_box s = print_string s
in

let print_option = Common.do_option
in
let print_between = Common.print_between
in

(* --------------------------------------------------------------------- *)

let handle_metavar name fn = 
  match (Common.optionise (fun () -> List.assoc (term name) env)) with
  | None -> failwith ("Not found a value in env for: " ^ term name)
  | Some e  -> fn e
in

(* --------------------------------------------------------------------- *)

(* Here we don't care about the annotation on s. *)
let mcode fn = function ((s,_,_)) -> fn s 
in
  


(* --------------------------------------------------------------------- *)
let dots between fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) -> print_between between fn l
  | Ast.CIRCLES(l) -> print_between between fn l
  | Ast.STARS(l) -> print_between between fn l
in



(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
    Ast.Id(name) -> mcode print_string name
  | Ast.MetaId(name) -> 
      handle_metavar name (function
        | (Ast_c.MetaIdVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.MetaFunc(name) -> 
      handle_metavar name (function
        | (Ast_c.MetaFuncVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.MetaLocalFunc(name) -> 
      handle_metavar name (function
        | (Ast_c.MetaLocalFuncVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.OptIdent(id) | Ast.UniqueIdent(id) | Ast.MultiIdent(id) -> 
      raise CantBeInPlus

in

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  match Ast.unwrap e with
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> mcode constant const
  | Ast.FunCall(fn,lp,args,rp) ->
      expression fn; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp

  | Ast.MetaExpr (name,_typedontcare) -> 
      handle_metavar name  (function
        | Ast_c.MetaExprVal exp -> 
            Pretty_print_c.pp_expression_gen pr_elem  exp
        | _ -> raise Impossible
                           )
          
          

  | Ast.EComma(cm) -> mcode print_string cm; print_space()

          
  | Ast.DisjExpr _ -> raise CantBeInPlus
  | Ast.Edots _ -> raise CantBeInPlus

  | _ -> raise Todo

and  unaryOp = function
    Ast.GetRef -> print_string "&"
  | Ast.DeRef -> print_string "*"
  | Ast.UnPlus -> print_string "+"
  | Ast.UnMinus -> print_string "-"
  | Ast.Tilde -> print_string "~"
  | Ast.Not -> print_string "!"

and  assignOp = function
    Ast.SimpleAssign -> print_string "="
  | Ast.OpAssign(aop) -> arithOp aop; print_string "="

and  fixOp = function
    Ast.Dec -> print_string "--"
  | Ast.Inc -> print_string "++"

and  binaryOp = function
    Ast.Arith(aop) -> arithOp aop
  | Ast.Logical(lop) -> logicalOp lop

and  arithOp = function
    Ast.Plus -> print_string "+"
  | Ast.Minus -> print_string "-"
  | Ast.Mul -> print_string "*"
  | Ast.Div -> print_string "/"
  | Ast.Mod -> print_string "%"
  | Ast.DecLeft -> print_string "<<"
  | Ast.DecRight -> print_string ">>"
  | Ast.And -> print_string "&"
  | Ast.Or -> print_string "|"
  | Ast.Xor -> print_string "^"

and  logicalOp = function
    Ast.Inf -> print_string "<"
  | Ast.Sup -> print_string ">"
  | Ast.InfEq -> print_string "<="
  | Ast.SupEq -> print_string ">="
  | Ast.Eq -> print_string "=="
  | Ast.NotEq -> print_string "!="
  | Ast.AndLog -> print_string "&&"
  | Ast.OrLog -> print_string "||"

and constant = function
    Ast.String(s) -> print_string "\""; print_string s; print_string "\""
  | Ast.Char(s) -> print_string s
  | Ast.Int(s) -> print_string s
  | Ast.Float(s) -> print_string s

(* --------------------------------------------------------------------- *)
(* Types *)


and fullType ft =
  match Ast.unwrap ft with
    Ast.Type(cv,ty) ->
      print_option (function x -> mcode const_vol x; print_string " ") cv;
      typeC ty
  | Ast.OptType(ty) | Ast.UniqueType(ty) | Ast.MultiType(ty) -> 
      raise CantBeInPlus

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,sgn) -> mcode baseType ty; print_option (mcode sign) sgn
  | Ast.Pointer(ty,star) -> fullType ty; mcode print_string star
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.StructUnionName(name,kind) ->
      mcode structUnion kind; mcode print_string name; print_string " "
  | Ast.TypeName(name)-> mcode print_string name; print_string " "
  | Ast.MetaType(name)-> raise Todo


and baseType = function
    Ast.VoidType -> print_string "void "
  | Ast.CharType -> print_string "char "
  | Ast.ShortType -> print_string "short "
  | Ast.IntType -> print_string "int "
  | Ast.DoubleType -> print_string "double "
  | Ast.FloatType -> print_string "float "
  | Ast.LongType -> print_string "long "

and structUnion = function
    Ast.Struct -> print_string "struct "
  | Ast.Union -> print_string "union "

and sign = function
    Ast.Signed -> print_string "signed "
  | Ast.Unsigned -> print_string "unsigned "


and const_vol = function
    Ast.Const -> print_string "const"
  | Ast.Volatile -> print_string "volatile"
in          

(* --------------------------------------------------------------------- *)
(* Variable declaration *)

let rec declaration d =
  match Ast.unwrap d with
  | _ -> raise Todo
in

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> fullType ty
  | Ast.Param(id,ty) -> fullType ty; ident id
  | Ast.MetaParam(name) -> 
      raise Todo
  | Ast.MetaParamList(name) -> 
      raise Todo
  | Ast.PComma(cm) -> mcode print_string cm; print_space()

  | Ast.Pdots(dots) -> raise Todo
  | Ast.Pcircles(dots) -> raise Todo
  | Ast.OptParam(param) | Ast.UniqueParam(param) -> raise CantBeInPlus
in


let parameter_list = dots (function _ -> ()) parameterTypeDef
in


(* --------------------------------------------------------------------- *)
(* Function declaration *)

let storage Ast.Static = print_string "static "
in

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rule_elem arity re =
  match Ast.unwrap re with

  | Ast.ExprStatement(exp,sem) ->
      print_string arity; expression exp; mcode print_string sem
  | Ast.Exp(exp) -> print_string arity; expression exp

  | Ast.SeqStart(brace) ->
      print_string arity; mcode print_string brace; start_block()

  | Ast.SeqEnd(brace) ->
      end_block(); print_string arity; mcode print_string brace

  | Ast.IfHeader(iff,lp,exp,rp) ->
      print_string arity;
      mcode print_string iff; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "

  | Ast.Else(els) ->
      print_string arity; mcode print_string els; print_string " "

  | Ast.MetaStmt(name) ->
      handle_metavar name  (function
        | Ast_c.MetaStmtVal exp -> 
            Pretty_print_c.pp_statement_gen pr_elem  exp
        | _ -> raise Impossible
                           )
        
  | _ -> raise Todo
in
          


let rec statement arity s =
  match Ast.unwrap s with

  | Ast.Atomic(re) -> rule_elem arity re

  | _ -> raise Todo
in

let top_level t =
  match Ast.unwrap t with

  | Ast.CODE(stmt_dots) ->
      dots force_newline (statement "") stmt_dots

  | _ -> raise Todo
in

(*    
let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level
in
*)


let rec pp_any = function
  (* assert: normally there is only CONTEXT NOTHING tokens in any *)
    Ast.FullTypeTag(x) -> fullType x
  | Ast.BaseTypeTag(x) -> baseType x
  | Ast.StructUnionTag(x) -> structUnion x
  | Ast.SignTag(x) -> sign x

  | Ast.IdentTag(x) -> ident x

  | Ast.ExpressionTag(x) -> expression x

  | Ast.ConstantTag(x) -> constant x
  | Ast.UnaryOpTag(x) -> unaryOp x
  | Ast.AssignOpTag(x) -> assignOp x
  | Ast.FixOpTag(x) -> fixOp x
  | Ast.BinaryOpTag(x) -> binaryOp x
  | Ast.ArithOpTag(x) -> arithOp x
  | Ast.LogicalOpTag(x) -> logicalOp x

  | Ast.DeclarationTag(x) -> declaration x
  | Ast.ParameterTypeDefTag(x) -> parameterTypeDef x

  | Ast.StorageTag(x) -> storage x

  | Ast.Rule_elemTag(x) -> rule_elem "" x
  | Ast.StatementTag(x) -> statement "" x

  | Ast.ConstVolTag(x) -> const_vol x
  | Ast.Token(x) -> print_string x
  | Ast.Code(x) -> let _ = top_level x in ()

  (* this not '...', but a list of expr/statement/params, and 
     normally there should be no '...' inside them *)
  | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x
  | Ast.ParamDotsTag(x) -> parameter_list x
  | Ast.StmtDotsTag(x) -> dots (function _ -> ()) (statement "") x

  | Ast.TypeCTag(x) -> typeC x
  | Ast.ParamTag(x) -> parameterTypeDef x
in

  (* todo? imitate what is in unparse_cocci ? *)
  match xxs with
  | xs::xxs -> 
      xs +> List.iter (fun any -> 
        (match any with
        | Ast.Rule_elemTag _ -> pr "\n"; pr current_tabbing;
        | Ast.StatementTag _ -> pr "\n"; pr current_tabbing;
        | _ -> ()
        );

        pp_any any
                      );
      xxs +> List.iter (fun xs -> 
        pr "\n"; 
        pr current_tabbing;
        xs +> List.iter (fun any -> 
          pp_any any
                        ); 
                       )
  | [] -> ()


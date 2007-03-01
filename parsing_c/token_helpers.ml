open Common open Commonop

open Parser_c

let is_comment = function
  | TComment _    | TCommentSpace _ 
  | TCommentCpp _ | TCommentMisc _ -> true
  | _ -> false

let is_not_comment x = not (is_comment x)


let is_cpp_instruction = function
  | TInclude _ | TDefine _
  | TIfdef _   | TIfdefelse _ | TIfdefelif _
  | TEndif _ 
  | TIfdefbool _ 
      -> true
  | _ -> false




(* Because ocamlyacc force us to do it that way. The ocamlyacc token 
 * cant be a pair of a sum type, it must be directly a sum type.
 *)
let info_from_token = function
  | TString ((string, isWchar), i) -> i
  | TChar  ((string, isWchar), i) -> i
  | TFloat ((string, floatType), i) -> i
  | TAssign  (assignOp, i) -> i
  | TIdent  (s, i) -> i
  | TypedefIdent  (s, i) -> i
  | TInt  (s, i) -> i

  (* I take only the first info, the next one are fakeInfo *)
  | TDefine (s, body, i1, i2, i3) -> 
      i1
  | TInclude (s, i1, i2) -> 
      i1

  | TUnknown             (i) -> i
  | TMacro             (i) -> i
  | TAction             (i) -> i

  | TComment             (i) -> i
  | TCommentSpace        (i) -> i
  | TCommentCpp          (i) -> i
  | TCommentMisc         (i) -> i
  | TIfdef               (i) -> i
  | TIfdefelse           (i) -> i
  | TIfdefelif           (i) -> i
  | TEndif               (i) -> i
  | TIfdefbool           (b, i) -> i
  | TOPar                (i) -> i
  | TCPar                (i) -> i
  | TOBrace              (i) -> i
  | TCBrace              (i) -> i
  | TOCro                (i) -> i
  | TCCro                (i) -> i
  | TDot                 (i) -> i
  | TComma               (i) -> i
  | TPtrOp               (i) -> i
  | TInc                 (i) -> i
  | TDec                 (i) -> i
  | TEq                  (i) -> i
  | TWhy                 (i) -> i
  | TTilde               (i) -> i
  | TBang                (i) -> i
  | TEllipsis            (i) -> i
  | TDotDot              (i) -> i
  | TPtVirg              (i) -> i
  | TOrLog               (i) -> i
  | TAndLog              (i) -> i
  | TOr                  (i) -> i
  | TXor                 (i) -> i
  | TAnd                 (i) -> i
  | TEqEq                (i) -> i
  | TNotEq               (i) -> i
  | TInf                 (i) -> i
  | TSup                 (i) -> i
  | TInfEq               (i) -> i
  | TSupEq               (i) -> i
  | TShl                 (i) -> i
  | TShr                 (i) -> i
  | TPlus                (i) -> i
  | TMinus               (i) -> i
  | TMul                 (i) -> i
  | TDiv                 (i) -> i
  | TMod                 (i) -> i
  | Tchar                (i) -> i
  | Tshort               (i) -> i
  | Tint                 (i) -> i
  | Tdouble              (i) -> i
  | Tfloat               (i) -> i
  | Tlong                (i) -> i
  | Tunsigned            (i) -> i
  | Tsigned              (i) -> i
  | Tvoid                (i) -> i
  | Tauto                (i) -> i
  | Tregister            (i) -> i
  | Textern              (i) -> i
  | Tstatic              (i) -> i
  | Tconst               (i) -> i
  | Tvolatile            (i) -> i
  | Tstruct              (i) -> i
  | Tenum                (i) -> i
  | Ttypedef             (i) -> i
  | Tunion               (i) -> i
  | Tbreak               (i) -> i
  | Telse                (i) -> i
  | Tswitch              (i) -> i
  | Tcase                (i) -> i
  | Tcontinue            (i) -> i
  | Tfor                 (i) -> i
  | Tdo                  (i) -> i
  | Tif                  (i) -> i
  | Twhile               (i) -> i
  | Treturn              (i) -> i
  | Tgoto                (i) -> i
  | Tdefault             (i) -> i
  | Tsizeof              (i) -> i
  | Tasm                 (i) -> i
  | Tattribute           (i) -> i
  | Tinline              (i) -> i
  | EOF                  (i) -> i
  


(* used by tokens to complete the parse_info with filename, line, col infos *)
let visitor_info_from_token f = function
  | TString ((string, isWchar), i) -> 
      TString ((string, isWchar), f i) 
  | TChar  ((string, isWchar), i) -> 
      TChar  ((string, isWchar), f i) 
  | TFloat ((string, floatType), i) -> 
      TFloat ((string, floatType), f i) 
  | TAssign  (assignOp, i) -> 
      TAssign  (assignOp, f i) 

  | TIdent  (s, i) -> 
      TIdent  (s, f i) 
  | TypedefIdent  (s, i) -> 
      TypedefIdent  (s, f i) 
  | TInt  (s, i) -> 
      TInt  (s, f i) 

  (* Don't visit, fakeInfo cant have better line x col info *)
  | TDefine (s, body, i1, i2, i3) -> 
      TDefine (s, body, f i1, i2, i3)
  | TInclude (s, i1, i2) -> 
      TInclude (s, f i1, i2)

  | TUnknown             (i) -> TUnknown             (f i)
  | TMacro               (i) -> TMacro             (f i)
  | TAction               (i) -> TAction             (f i)

  | TComment             (i) -> TComment             (f i) 
  | TCommentSpace        (i) -> TCommentSpace        (f i) 
  | TCommentCpp          (i) -> TCommentCpp          (f i) 
  | TCommentMisc         (i) -> TCommentMisc         (f i) 
  | TIfdef               (i) -> TIfdef               (f i) 
  | TIfdefelse           (i) -> TIfdefelse           (f i) 
  | TIfdefelif           (i) -> TIfdefelif           (f i) 
  | TEndif               (i) -> TEndif               (f i) 
  | TIfdefbool           (b, i) -> TIfdefbool    (b, f i) 
  | TOPar                (i) -> TOPar                (f i) 
  | TCPar                (i) -> TCPar                (f i) 
  | TOBrace              (i) -> TOBrace              (f i) 
  | TCBrace              (i) -> TCBrace              (f i) 
  | TOCro                (i) -> TOCro                (f i) 
  | TCCro                (i) -> TCCro                (f i) 
  | TDot                 (i) -> TDot                 (f i) 
  | TComma               (i) -> TComma               (f i) 
  | TPtrOp               (i) -> TPtrOp               (f i) 
  | TInc                 (i) -> TInc                 (f i) 
  | TDec                 (i) -> TDec                 (f i) 
  | TEq                  (i) -> TEq                  (f i) 
  | TWhy                 (i) -> TWhy                 (f i) 
  | TTilde               (i) -> TTilde               (f i) 
  | TBang                (i) -> TBang                (f i) 
  | TEllipsis            (i) -> TEllipsis            (f i) 
  | TDotDot              (i) -> TDotDot              (f i) 
  | TPtVirg              (i) -> TPtVirg              (f i) 
  | TOrLog               (i) -> TOrLog               (f i) 
  | TAndLog              (i) -> TAndLog              (f i) 
  | TOr                  (i) -> TOr                  (f i) 
  | TXor                 (i) -> TXor                 (f i) 
  | TAnd                 (i) -> TAnd                 (f i) 
  | TEqEq                (i) -> TEqEq                (f i) 
  | TNotEq               (i) -> TNotEq               (f i) 
  | TInf                 (i) -> TInf                 (f i) 
  | TSup                 (i) -> TSup                 (f i) 
  | TInfEq               (i) -> TInfEq               (f i) 
  | TSupEq               (i) -> TSupEq               (f i) 
  | TShl                 (i) -> TShl                 (f i) 
  | TShr                 (i) -> TShr                 (f i) 
  | TPlus                (i) -> TPlus                (f i) 
  | TMinus               (i) -> TMinus               (f i) 
  | TMul                 (i) -> TMul                 (f i) 
  | TDiv                 (i) -> TDiv                 (f i) 
  | TMod                 (i) -> TMod                 (f i) 
  | Tchar                (i) -> Tchar                (f i) 
  | Tshort               (i) -> Tshort               (f i) 
  | Tint                 (i) -> Tint                 (f i) 
  | Tdouble              (i) -> Tdouble              (f i) 
  | Tfloat               (i) -> Tfloat               (f i) 
  | Tlong                (i) -> Tlong                (f i) 
  | Tunsigned            (i) -> Tunsigned            (f i) 
  | Tsigned              (i) -> Tsigned              (f i) 
  | Tvoid                (i) -> Tvoid                (f i) 
  | Tauto                (i) -> Tauto                (f i) 
  | Tregister            (i) -> Tregister            (f i) 
  | Textern              (i) -> Textern              (f i) 
  | Tstatic              (i) -> Tstatic              (f i) 
  | Tconst               (i) -> Tconst               (f i) 
  | Tvolatile            (i) -> Tvolatile            (f i) 
  | Tstruct              (i) -> Tstruct              (f i) 
  | Tenum                (i) -> Tenum                (f i) 
  | Ttypedef             (i) -> Ttypedef             (f i) 
  | Tunion               (i) -> Tunion               (f i) 
  | Tbreak               (i) -> Tbreak               (f i) 
  | Telse                (i) -> Telse                (f i) 
  | Tswitch              (i) -> Tswitch              (f i) 
  | Tcase                (i) -> Tcase                (f i) 
  | Tcontinue            (i) -> Tcontinue            (f i) 
  | Tfor                 (i) -> Tfor                 (f i) 
  | Tdo                  (i) -> Tdo                  (f i) 
  | Tif                  (i) -> Tif                  (f i) 
  | Twhile               (i) -> Twhile               (f i) 
  | Treturn              (i) -> Treturn              (f i) 
  | Tgoto                (i) -> Tgoto                (f i) 
  | Tdefault             (i) -> Tdefault             (f i) 
  | Tsizeof              (i) -> Tsizeof              (f i) 
  | Tasm                 (i) -> Tasm                 (f i) 
  | Tattribute           (i) -> Tattribute           (f i) 
  | Tinline              (i) -> Tinline              (f i) 
  | EOF                  (i) -> EOF                  (f i) 
  


let linecol_of_tok tok =
  let (parse_info,_cocci_info) = info_from_token tok in
  parse_info.Common.line, parse_info.Common.column





let info_from_token_fullstr x = 

  match x with 
  | TDefine (s, body, i1, i2, i3) -> 
      let (info, annot) = i1 in
      { info with 
        Common.str = (fst i1).str ^ (fst i2).str ^ (fst i3).str
      }, annot
  | TInclude (s, i1, i2) -> 
      let (info, annot) = i1 in
      { info with 
        Common.str = (fst i1).str ^ (fst i2).str
      }, annot
  | x -> info_from_token x

let pos_of_token x = 
  Ast_c.get_pos_of_info (info_from_token_fullstr x)

let str_of_token x = 
  Ast_c.get_str_of_info (info_from_token_fullstr x)



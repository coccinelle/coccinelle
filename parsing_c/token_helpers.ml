open Common open Commonop

let is_comment = function
  | Parser_c.TComment _  | Parser_c.TCommentSpace _ 
  | Parser_c.TCommentCpp _ | Parser_c.TCommentMisc _ -> true
  | _ -> false

let is_not_comment x = not (is_comment x)


let is_cpp_instruction = function
  | Parser_c.TInclude _ | Parser_c.TDefine _
  | Parser_c.TIfdef _ | Parser_c.TIfdefelse _ | Parser_c.TIfdefelif _
  | Parser_c.TEndif _ 
  | Parser_c.TIfdefzero _ 
      -> true
  | _ -> false




(* Because ocamlyacc force us to do it that way. The ocamlyacc token 
 * cant be a pair of a sum type, it must be directly a sum type.
 *)
let info_from_token = function
  | Parser_c.TString ((string, isWchar), i) -> i
  | Parser_c.TChar  ((string, isWchar), i) -> i
  | Parser_c.TFloat ((string, floatType), i) -> i
  | Parser_c.TAssign  (assignOp, i) -> i
  | Parser_c.TIdent  (s, i) -> i
  | Parser_c.TypedefIdent  (s, i) -> i
  | Parser_c.TInt  (s, i) -> i

  (* I take only the first info, the next one are fakeInfo *)
  | Parser_c.TDefine (s, body, i1, i2, i3) -> 
      i1
  | Parser_c.TInclude (s, i1, i2) -> 
      i1

  | Parser_c.TComment             (i) -> i
  | Parser_c.TCommentSpace        (i) -> i
  | Parser_c.TCommentCpp          (i) -> i
  | Parser_c.TCommentMisc         (i) -> i
  | Parser_c.TIfdef               (i) -> i
  | Parser_c.TIfdefelse           (i) -> i
  | Parser_c.TIfdefelif           (i) -> i
  | Parser_c.TEndif               (i) -> i
  | Parser_c.TIfdefzero           (i) -> i
  | Parser_c.TOPar                (i) -> i
  | Parser_c.TCPar                (i) -> i
  | Parser_c.TOBrace              (i) -> i
  | Parser_c.TCBrace              (i) -> i
  | Parser_c.TOCro                (i) -> i
  | Parser_c.TCCro                (i) -> i
  | Parser_c.TDot                 (i) -> i
  | Parser_c.TComma               (i) -> i
  | Parser_c.TPtrOp               (i) -> i
  | Parser_c.TInc                 (i) -> i
  | Parser_c.TDec                 (i) -> i
  | Parser_c.TEq                  (i) -> i
  | Parser_c.TWhy                 (i) -> i
  | Parser_c.TTilde               (i) -> i
  | Parser_c.TBang                (i) -> i
  | Parser_c.TEllipsis            (i) -> i
  | Parser_c.TDotDot              (i) -> i
  | Parser_c.TPtVirg              (i) -> i
  | Parser_c.TOrLog               (i) -> i
  | Parser_c.TAndLog              (i) -> i
  | Parser_c.TOr                  (i) -> i
  | Parser_c.TXor                 (i) -> i
  | Parser_c.TAnd                 (i) -> i
  | Parser_c.TEqEq                (i) -> i
  | Parser_c.TNotEq               (i) -> i
  | Parser_c.TInf                 (i) -> i
  | Parser_c.TSup                 (i) -> i
  | Parser_c.TInfEq               (i) -> i
  | Parser_c.TSupEq               (i) -> i
  | Parser_c.TShl                 (i) -> i
  | Parser_c.TShr                 (i) -> i
  | Parser_c.TPlus                (i) -> i
  | Parser_c.TMinus               (i) -> i
  | Parser_c.TMul                 (i) -> i
  | Parser_c.TDiv                 (i) -> i
  | Parser_c.TMod                 (i) -> i
  | Parser_c.Tchar                (i) -> i
  | Parser_c.Tshort               (i) -> i
  | Parser_c.Tint                 (i) -> i
  | Parser_c.Tdouble              (i) -> i
  | Parser_c.Tfloat               (i) -> i
  | Parser_c.Tlong                (i) -> i
  | Parser_c.Tunsigned            (i) -> i
  | Parser_c.Tsigned              (i) -> i
  | Parser_c.Tvoid                (i) -> i
  | Parser_c.Tauto                (i) -> i
  | Parser_c.Tregister            (i) -> i
  | Parser_c.Textern              (i) -> i
  | Parser_c.Tstatic              (i) -> i
  | Parser_c.Tconst               (i) -> i
  | Parser_c.Tvolatile            (i) -> i
  | Parser_c.Tstruct              (i) -> i
  | Parser_c.Tenum                (i) -> i
  | Parser_c.Ttypedef             (i) -> i
  | Parser_c.Tunion               (i) -> i
  | Parser_c.Tbreak               (i) -> i
  | Parser_c.Telse                (i) -> i
  | Parser_c.Tswitch              (i) -> i
  | Parser_c.Tcase                (i) -> i
  | Parser_c.Tcontinue            (i) -> i
  | Parser_c.Tfor                 (i) -> i
  | Parser_c.Tdo                  (i) -> i
  | Parser_c.Tif                  (i) -> i
  | Parser_c.Twhile               (i) -> i
  | Parser_c.Treturn              (i) -> i
  | Parser_c.Tgoto                (i) -> i
  | Parser_c.Tdefault             (i) -> i
  | Parser_c.Tsizeof              (i) -> i
  | Parser_c.Tasm                 (i) -> i
  | Parser_c.Tattribute           (i) -> i
  | Parser_c.Tinline              (i) -> i
  | Parser_c.EOF                  (i) -> i
  


(* used by tokens to complete the parse_info with filename, line, col infos *)
let visitor_info_from_token f = function
  | Parser_c.TString ((string, isWchar), i) -> 
      Parser_c.TString ((string, isWchar), f i) 
  | Parser_c.TChar  ((string, isWchar), i) -> 
      Parser_c.TChar  ((string, isWchar), f i) 
  | Parser_c.TFloat ((string, floatType), i) -> 
      Parser_c.TFloat ((string, floatType), f i) 
  | Parser_c.TAssign  (assignOp, i) -> 
      Parser_c.TAssign  (assignOp, f i) 

  | Parser_c.TIdent  (s, i) -> 
      Parser_c.TIdent  (s, f i) 
  | Parser_c.TypedefIdent  (s, i) -> 
      Parser_c.TypedefIdent  (s, f i) 
  | Parser_c.TInt  (s, i) -> 
      Parser_c.TInt  (s, f i) 

  (* Don't visit, fakeInfo cant have better line x col info *)
  | Parser_c.TDefine (s, body, i1, i2, i3) -> 
      Parser_c.TDefine (s, body, f i1, i2, i3)
  | Parser_c.TInclude (s, i1, i2) -> 
      Parser_c.TInclude (s, f i1, i2)

  | Parser_c.TComment             (i) -> Parser_c.TComment             (f i) 
  | Parser_c.TCommentSpace        (i) -> Parser_c.TCommentSpace        (f i) 
  | Parser_c.TCommentCpp          (i) -> Parser_c.TCommentCpp          (f i) 
  | Parser_c.TCommentMisc         (i) -> Parser_c.TCommentMisc         (f i) 
  | Parser_c.TIfdef               (i) -> Parser_c.TIfdef               (f i) 
  | Parser_c.TIfdefelse           (i) -> Parser_c.TIfdefelse           (f i) 
  | Parser_c.TIfdefelif           (i) -> Parser_c.TIfdefelif           (f i) 
  | Parser_c.TEndif               (i) -> Parser_c.TEndif               (f i) 
  | Parser_c.TIfdefzero           (i) -> Parser_c.TIfdefzero           (f i) 
  | Parser_c.TOPar                (i) -> Parser_c.TOPar                (f i) 
  | Parser_c.TCPar                (i) -> Parser_c.TCPar                (f i) 
  | Parser_c.TOBrace              (i) -> Parser_c.TOBrace              (f i) 
  | Parser_c.TCBrace              (i) -> Parser_c.TCBrace              (f i) 
  | Parser_c.TOCro                (i) -> Parser_c.TOCro                (f i) 
  | Parser_c.TCCro                (i) -> Parser_c.TCCro                (f i) 
  | Parser_c.TDot                 (i) -> Parser_c.TDot                 (f i) 
  | Parser_c.TComma               (i) -> Parser_c.TComma               (f i) 
  | Parser_c.TPtrOp               (i) -> Parser_c.TPtrOp               (f i) 
  | Parser_c.TInc                 (i) -> Parser_c.TInc                 (f i) 
  | Parser_c.TDec                 (i) -> Parser_c.TDec                 (f i) 
  | Parser_c.TEq                  (i) -> Parser_c.TEq                  (f i) 
  | Parser_c.TWhy                 (i) -> Parser_c.TWhy                 (f i) 
  | Parser_c.TTilde               (i) -> Parser_c.TTilde               (f i) 
  | Parser_c.TBang                (i) -> Parser_c.TBang                (f i) 
  | Parser_c.TEllipsis            (i) -> Parser_c.TEllipsis            (f i) 
  | Parser_c.TDotDot              (i) -> Parser_c.TDotDot              (f i) 
  | Parser_c.TPtVirg              (i) -> Parser_c.TPtVirg              (f i) 
  | Parser_c.TOrLog               (i) -> Parser_c.TOrLog               (f i) 
  | Parser_c.TAndLog              (i) -> Parser_c.TAndLog              (f i) 
  | Parser_c.TOr                  (i) -> Parser_c.TOr                  (f i) 
  | Parser_c.TXor                 (i) -> Parser_c.TXor                 (f i) 
  | Parser_c.TAnd                 (i) -> Parser_c.TAnd                 (f i) 
  | Parser_c.TEqEq                (i) -> Parser_c.TEqEq                (f i) 
  | Parser_c.TNotEq               (i) -> Parser_c.TNotEq               (f i) 
  | Parser_c.TInf                 (i) -> Parser_c.TInf                 (f i) 
  | Parser_c.TSup                 (i) -> Parser_c.TSup                 (f i) 
  | Parser_c.TInfEq               (i) -> Parser_c.TInfEq               (f i) 
  | Parser_c.TSupEq               (i) -> Parser_c.TSupEq               (f i) 
  | Parser_c.TShl                 (i) -> Parser_c.TShl                 (f i) 
  | Parser_c.TShr                 (i) -> Parser_c.TShr                 (f i) 
  | Parser_c.TPlus                (i) -> Parser_c.TPlus                (f i) 
  | Parser_c.TMinus               (i) -> Parser_c.TMinus               (f i) 
  | Parser_c.TMul                 (i) -> Parser_c.TMul                 (f i) 
  | Parser_c.TDiv                 (i) -> Parser_c.TDiv                 (f i) 
  | Parser_c.TMod                 (i) -> Parser_c.TMod                 (f i) 
  | Parser_c.Tchar                (i) -> Parser_c.Tchar                (f i) 
  | Parser_c.Tshort               (i) -> Parser_c.Tshort               (f i) 
  | Parser_c.Tint                 (i) -> Parser_c.Tint                 (f i) 
  | Parser_c.Tdouble              (i) -> Parser_c.Tdouble              (f i) 
  | Parser_c.Tfloat               (i) -> Parser_c.Tfloat               (f i) 
  | Parser_c.Tlong                (i) -> Parser_c.Tlong                (f i) 
  | Parser_c.Tunsigned            (i) -> Parser_c.Tunsigned            (f i) 
  | Parser_c.Tsigned              (i) -> Parser_c.Tsigned              (f i) 
  | Parser_c.Tvoid                (i) -> Parser_c.Tvoid                (f i) 
  | Parser_c.Tauto                (i) -> Parser_c.Tauto                (f i) 
  | Parser_c.Tregister            (i) -> Parser_c.Tregister            (f i) 
  | Parser_c.Textern              (i) -> Parser_c.Textern              (f i) 
  | Parser_c.Tstatic              (i) -> Parser_c.Tstatic              (f i) 
  | Parser_c.Tconst               (i) -> Parser_c.Tconst               (f i) 
  | Parser_c.Tvolatile            (i) -> Parser_c.Tvolatile            (f i) 
  | Parser_c.Tstruct              (i) -> Parser_c.Tstruct              (f i) 
  | Parser_c.Tenum                (i) -> Parser_c.Tenum                (f i) 
  | Parser_c.Ttypedef             (i) -> Parser_c.Ttypedef             (f i) 
  | Parser_c.Tunion               (i) -> Parser_c.Tunion               (f i) 
  | Parser_c.Tbreak               (i) -> Parser_c.Tbreak               (f i) 
  | Parser_c.Telse                (i) -> Parser_c.Telse                (f i) 
  | Parser_c.Tswitch              (i) -> Parser_c.Tswitch              (f i) 
  | Parser_c.Tcase                (i) -> Parser_c.Tcase                (f i) 
  | Parser_c.Tcontinue            (i) -> Parser_c.Tcontinue            (f i) 
  | Parser_c.Tfor                 (i) -> Parser_c.Tfor                 (f i) 
  | Parser_c.Tdo                  (i) -> Parser_c.Tdo                  (f i) 
  | Parser_c.Tif                  (i) -> Parser_c.Tif                  (f i) 
  | Parser_c.Twhile               (i) -> Parser_c.Twhile               (f i) 
  | Parser_c.Treturn              (i) -> Parser_c.Treturn              (f i) 
  | Parser_c.Tgoto                (i) -> Parser_c.Tgoto                (f i) 
  | Parser_c.Tdefault             (i) -> Parser_c.Tdefault             (f i) 
  | Parser_c.Tsizeof              (i) -> Parser_c.Tsizeof              (f i) 
  | Parser_c.Tasm                 (i) -> Parser_c.Tasm                 (f i) 
  | Parser_c.Tattribute           (i) -> Parser_c.Tattribute           (f i) 
  | Parser_c.Tinline              (i) -> Parser_c.Tinline              (f i) 
  | Parser_c.EOF                  (i) -> Parser_c.EOF                  (f i) 
  


let linecol_of_tok tok =
  let (parse_info,_cocci_info) = info_from_token tok in
  parse_info.Common.line, parse_info.Common.column




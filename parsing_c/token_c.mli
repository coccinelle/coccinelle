type cppcommentkind =
    CppDirective
  | CppIfDirective of ifdef
  | CppAttr
  | CppMacro
  | CppPassingNormal
  | CppPassingCosWouldGetError
  | CppPassingExplicit
and ifdef = IfDef | IfDef0 | Else | Endif | Other
type info = Common.parse_info
type token = token_tag * info
and token_tag =
    TCommentSpace
  | TCommentNewline
  | TComment
  | TCommentCpp of cppcommentkind
type comment_like_token = token
val info_of_token : 'a * 'b -> 'b
val str_of_token : 'a * Common.parse_info -> string

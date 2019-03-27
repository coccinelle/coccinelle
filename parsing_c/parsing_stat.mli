type parsing_stat = {
  filename : Common.filename;
  mutable have_timeout : bool;
  mutable correct : int;
  mutable bad : int;
  mutable commentized : int;
  mutable problematic_lines : (string list * int) list;
}
val default_stat : Common.filename -> parsing_stat
val print_parsing_stat_list : ?verbose:bool -> parsing_stat list -> unit
val lines_around_error_line :
  context:int -> Common.filename * int -> string list
val print_recurring_problematic_tokens : parsing_stat list -> unit
val nTypedefInfer : int ref
val nIncludeGrammar : int ref
val nIncludeHack : int ref
val nIteratorGrammar : int ref
val nIteratorHeuristic : int ref
val nMacroTopDecl : int ref
val nMacroStructDecl : int ref
val nMacroDecl : int ref
val nMacroStmt : int ref
val nMacroString : int ref
val nMacroHigherOrder : int ref
val nMacrohigherTypeGrammar : int ref
val nMacroAttribute : int ref
val nIfdefTop : int ref
val nIfdefStmt : int ref
val nIfdefStruct : int ref
val nIfdefInitializer : int ref
val nIfdefFunheader : int ref
val nIfdefExprPassing : int ref
val nIfdefPassing : int ref
val nPragmaPassing : int ref
val nIncludePassing : int ref
val nUndefPassing : int ref
val nDefinePassing : int ref
val nIfdefZero : int ref
val nIfdefVersion : int ref
val nGccTypeof : int ref
val nGccLongLong : int ref
val nGccAsm : int ref
val nGccInline : int ref
val nGccAttribute : int ref
val nGccCaseRange : int ref
val nGccMixDecl : int ref
val nGccDesignator : int ref
val nGccStmtExpr : int ref
val nGccConstructor : int ref
val nGccEmptyStruct : int ref
val nGccNestedFunc : int ref
val nGccMisc : int ref
val nDefineHack : int ref
val nDefineConstant : int ref
val nDefineStmt : int ref
val nDefineExpr : int ref
val nDefineWhile0 : int ref
val nDefineInit : int ref
val nDefineOther : int ref
val nUndef : int ref
val nOtherDirective : int ref
val nDirectiveStmt : int ref
val nDirectiveStruct : int ref
val nDirectiveInitializer : int ref
val nMacroHint : int ref
val nMacroExpand : int ref
val nNotParsedCorrectly : int ref
val assoc_stat_number : (string * int ref) list
val print_stat_numbers : unit -> unit

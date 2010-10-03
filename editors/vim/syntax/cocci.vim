" Vim syntax file
" Language:     Cocci (SmPL)
" Author:       Alexander Færøy <ahf@0x90.dk>
" Copyright:    Copyright (c) 2009 Alexander Færøy
" License:      You may redistribute this under the same terms as Vim itself.

if &compatible || v:version < 603 || exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword CocciKeywords       identifier type parameter constant expression contained
syn keyword CocciKeywords       statement function local list fresh position idexpression contained

syn region CocciGroup matchgroup=CocciGroupDelim start="@[^@]*@" end="@@" contains=CocciKeywords

syn match CocciLineRemoved      "^-.*"
syn match CocciLineAdded        "^+.*"
syn match CocciComment          "//.*"

syn case ignore
syn match CocciOperator         "\.\.\."
syn match CocciOperator         "when"
syn case match

" Errors
syn match CocciError            "^[ \t][+-].*"

" Highlight!
hi def link CocciLineRemoved    Special
hi def link CocciLineAdded      Identifier
hi def link CocciError          Error
hi def link CocciKeywords       Keyword
hi def link CocciGroupDelim     PreProc
hi def link CocciComment        Comment
hi def link CocciOperator       Operator

let b:current_syntax = "cocci"

" vim: set et ts=4 :

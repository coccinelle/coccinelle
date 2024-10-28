" Vim syntax file
" Language:     Cocci (SmPL)
" Author:       Alexander Færøy <ahf@0x90.dk>
" Copyright:    Copyright (c) 2009 Alexander Færøy
" License:      You may redistribute this under the same terms as Vim itself.

if &compatible || v:version < 603 || exists("b:current_syntax")
    finish
endif

" Highlighting python and ocaml blocks
syn include @PythonSyntax syntax/python.vim
syn include @OCamlSyntax syntax/ocaml.vim

syn region CocciPythonBlock matchgroup=CocciCodeBlock
    \ start=+^@\s*\(script\|initialize\|finalize\)\s*:\s*python\s*[^@]*@\(\n.*<<.*\)*\_s*@@+
    \ skip=+^$\|^\s*//.*$+
    \ end=+^\([^@]\)\@!+
    \ contains=@PythonSyntax,CocciComment
syn region CocciOCamlBlock matchgroup=CocciCodeBlock
    \ start=+@\s*\(script\|initialize\|finalize\)\s*:\s*ocaml\s*[^@]*@\(\n.*<<.*\)*\_s*@@+
    \ skip=+^$\|^\s*//.*$+
    \ end=+^\(\s*[^@]\)\@!+
    \ contains=@OCamlSyntax,CocciComment

" Keywords
syn keyword CocciKeywords       identifier type typedef parameter constant expression contained
syn keyword CocciKeywords       statement function local list fresh position idexpression contained
syn keyword CocciKeywords       declaration declarer attribute symbol format assignment contained
syn keyword CocciKeywords       operator global field initializer initialiser iterator name contained
syn keyword CocciKeywords       metavariable enum union struct binary unary const volatile ptrdiff_t contained
syn keyword CocciKeywords       signed unsigned void char short int long float double complex size_t ssize_t contained

syn region CocciGroup matchgroup=CocciGroupDelim start="^@[^@:]*@" end="@@" contains=CocciKeywords,CocciComment,CocciInlineScript

syn match CocciInlineScript     "script\s*:\s*\(python\|ocaml\)\s*([^)]*)\s*{[^}]*}" contained

syn region CocciLineRemoved start="^-"  end="$" keepend contains=CocciOperator,CocciPosition
syn region CocciLineAdded   start="^+"  end="$" keepend contains=CocciOperator,CocciPosition
syn region CocciLinePinned  start="^\*" end="$" keepend contains=CocciOperator,CocciPosition

syn match CocciComment          "//.*"
syn region CocciComment         start="/\*" end="\*/" extend

syn match CocciPosition         "\%(^\)\@!@[_a-zA-Z][_a-zA-Z0-9]*"
syn match CocciOperator         "\.\.\."
syn match CocciOperator         "<\.\.\."
syn match CocciOperator         "\.\.\.>"
syn match CocciOperator         "<+\.\.\."
syn match CocciOperator         "\.\.\.+>"
syn case ignore
syn match CocciOperator         "when"
syn match CocciOperator         "any"
syn case match

" Errors
syn match CocciError            "^[ \t][+-].*"

" Highlight!
hi def link CocciPosition       Directory
hi def link CocciLineRemoved    Special
hi def link CocciLineAdded      Identifier
hi def link CocciLinePinned     NonText
hi def link CocciError          Error
hi def link CocciKeywords       Type
hi def link CocciGroupDelim     PreProc
hi def link CocciComment        Comment
hi def link CocciOperator       Operator
hi def link CocciInlineScript   Special
hi def link CocciCodeBlock      PreProc

let b:current_syntax = "cocci"

" vim: set et ts=4 :

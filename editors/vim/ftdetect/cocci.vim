" Vim filetype detection file
" Language:     Cocci (SmPL)
" Author:       Alexander Færøy <ahf@0x90.dk>
" Copyright:    Copyright (c) 2009 Alexander Færøy
" License:      You may redistribute this under the same terms as Vim itself.

if &compatible || v:version < 603
    finish
endif

au BufNewFile,BufRead *.cocci set filetype=cocci

" vim: set et ts=4 :

" Vim syntax file
" Language:	CSS 3
" Maintainer: Shiao <i@shiao.org>
" Last Change:	2010 Apr 5

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'css'
endif

if version < 600
  so <sfile>:p:h/css.vim
else
  runtime! syntax/css.vim
  unlet b:current_syntax
endif

syn keyword cssColorProp contained opacity
syn match cssTextProp contained "\<word-wrap\>"
syn match cssTextProp contained "\<text-overflow\>"
syn match cssBoxProp contained "\<box-shadow\>"
syn match cssBoxProp contained "\<border-radius\>"
syn match cssBoxProp contained "\<border-\(\(top-left\|top-right\|bottom-right\|bottom-left\)-radius\)\>"
" firefox border-radius TODO
syn match cssBoxProp contained "-moz-border-radius\>"
syn match cssBoxProp contained "-moz-border-radius\(-\(bottomleft\|bottomright\|topright\|topleft\)\)\>"

let b:current_syntax = "css3"

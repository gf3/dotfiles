" markdown filetype file
if exists("did\_load\_filetypes")
 finish
endif
augroup markdown
 au! BufRead,BufNewFile *.mkd,*,md,*.mdown,*,markdown   setfiletype mkd
augroup END


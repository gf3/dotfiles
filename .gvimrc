set go-=T " Hide toolbar
set go-=r " Hide right scrollbar
set go-=L " Hide left scrollbar
set guifont=Menlo\ Regular\ for\ Powerline:h16 " Set default font

" Disable MacVim's new-tab functionality
macm File.New\ Tab key=<nop>

" Buffer Navigation
nmap <D-[> :bprevious<CR>
nmap <D-]> :bnext<CR>
map <D-[> :bprevious<CR>
map <D-]> :bnext<CR>
imap <D-[> <Esc>:bprevious<CR>i
imap <D-]> <Esc>:bnext<CR>i
nmap <D-t> :enew<CR>
imap <D-t> <Esc>:enew<CR>

" Toggle fullscreen
nmap <D-CR> :set invfu<CR>

" Maximize window
set lines=999 columns=999

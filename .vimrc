:syntax enable

:let mapleader=","

:set autoindent
:set backspace=indent,eol,start
:set foldmethod=syntax " Markers are used to specify folds.
:set foldenable
:set foldlevel=2
:set nowrap

" Speed up viewport scrolling.
:nnoremap <C-e> 3<C-e>
:nnoremap <C-y> 3<C-y>

" FuzzyFinder (,ff) (,fb) (,fd)
:nnoremap <leader>ff :FufFile<CR>
:nnoremap <leader>fb :FufBuffer<CR>
:nnoremap <leader>fd :FufDir<CR>

" New tab (Ctrl + T)
:map <C-T> <Esc>:tabnew<CR>

" Paste toggle (F2)
:nnoremap <F2> :set invpaste paste?<CR>
:imap <F2> <C-O><F2>
:set pastetoggle=<F2>

" Tabs
:set expandtab
:set tabstop=2

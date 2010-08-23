" Set syntax highlighting options.
set t_Co=256
set background=dark 
syntax on
colorscheme molokai

" Change mapleader
let mapleader=","

" Local dirs
set backupdir=~/.vim/backups
set directory=~/.vim/swaps

" Set some junk
set autoindent " Copy indent from last line when starting new line.
set backspace=indent,eol,start
set cursorline " Highlight current line
set diffopt=filler " Add vertical spaces to keep right and left aligned
set diffopt+=iwhite " Ignore whitespace changes (focus on code changes)
set encoding=utf-8 nobomb " BOM often causes trouble
set esckeys " Allow cursor keys in insert mode.
set expandtab " Expand tabs to spaces
set foldcolumn=4 " Column to show folds
set foldenable
set foldlevel=2
" set foldlevelstart=2 " Sets `foldlevel` when editing a new buffer
set foldmethod=indent " Markers are used to specify folds.
set foldnestmax=3 " Set max fold nesting level
set hidden " When a buffer is brought to foreground, remember undo history and marks.
set history=1000 " Increase history from 20 default to 1000
set hlsearch " Highlight searches
set incsearch " Highlight dynamically as pattern is typed.
set laststatus=2 " Always show status line
set magic " Enable extended regexes.
set mouse=a " Enable moouse in all in all modes.
set nocompatible " Make vim more useful
set noerrorbells " Disable error bells.
set nostartofline
set nowrap " Do not wrap lines.
set nu " Enable line numbers.
set ofu=syntaxcomplete#Complete " Set omni-completion method.
set report=0 " Show all changes.
set ruler " Show the cursor position
set scrolloff=3 " Start scrolling three lines before horizontal border of window.
set shiftwidth=2 " The # of spaces for indenting.
set shortmess=I " Don't show the intro message when starting vim.
set showmode " Show the current mode.
set showtabline=2 " Always show tab bar.
set smarttab " At start of line, <Tab> inserts shiftwidth spaces, <Bs> deletes shiftwidth spaces.
set softtabstop=2 " Tab key results in 2 spaces
set title " Show the filename in the window titlebar.
set ttyfast " Send more characters at a given time.
set ttymouse=xterm " Set mouse type to xterm.
set wildchar=<TAB> " Character for CLI expansion (TAB-completion).
set wildignore+=*.o,*.obj,*.min.js,smarty/**,vendor/rails/**,vendor/plugins/**,vendor/gems/**,.git,.hg,.svn,.sass-cache,log,tmp,build,_SCRIPTS,_TESTS
set wildmenu " Hitting TAB in command mode will show possible completions above command line.
set wildmode=list:longest " Complete only until point of ambiguity.

" Speed up viewport scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Remap arrow keys
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>

" Faster split resizing (+,-)
if bufwinnr(1)
  map + <C-W>+
  map - <C-W>-
endif

" Sudo write (,W)
noremap <leader>W :w !sudo tee %<CR>

" Remap :W to :w
command W w

" Toggle show tabs and trailing spaces (,c)
set lcs=tab:›\ ,trail:·,eol:↴,nbsp:_
set fcs=fold:-
nnoremap <silent> <leader>c :set nolist!<CR>

" FuzzyFinder (,ff) (,fb) (,fd)
nnoremap <leader>ff :FufFile<CR>
nnoremap <leader>fb :FufBuffer<CR>
nnoremap <leader>fd :FufDir<CR>

" Clear last search (,cs)
map <silent> <leader>cs <Esc>:noh<CR>
" map <silent> <leader>cs <Esc>:let @/ = ""<CR>

" Remap keys for auto-completion
inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"

" Indent/unident block (,]) (,[)
nnoremap <leader>] >i{<CR>
nnoremap <leader>[ <i{<CR>

" Paste toggle (,p)
set pastetoggle=<leader>p
map <leader>p :set invpaste paste?<CR>

" Nerdtree (,n)
map <leader>n :NERDTreeToggle<CR>

" Buffer navigation (,,) (,]) (,[) (,ls)
map <Leader>, <C-^>
" :map <Leader>] :bnext<CR>
" :map <Leader>[ :bprev<CR>
map <Leader>ls :buffers<CR>

" Close Quickfix window (,cc)
map <leader>cc :cclose<CR>

" Yank from cursor to end of line
nnoremap Y y$

" Insert newline
map <leader><Enter> o<ESC>

" Strip trailing whitespace (,ss)
function! StripWhitespace ()
    exec ':%s/ \+$//gc'
endfunction
noremap <leader>ss :call StripWhitespace ()<CR>

" Save and restore folds
" :au BufWinLeave * mkview
" :au BufWinEnter * silent loadview

" Fix page up and down
map <PageUp> <C-U>
map <PageDown> <C-D>
imap <PageUp> <C-O><C-U>
imap <PageDown> <C-O><C-D>

filetype plugin indent on

" Restore cursor position
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif

" Emulate bundles, allow plugins to live independantly. Easier to manage.
call pathogen#runtime_append_all_bundles()

" Markdown
augroup mkd
  autocmd BufRead *.mkd  set ai formatoptions=tcroqn2 comments=n:>
augroup END

" CSS3
au BufRead,BufNewFile *.css set ft=css syntax=css3

" HTML5
au BufRead,BufNewFile *.html set ft=html syntax=html5

" Command-T
let g:CommandTMaxFiles=20000
let g:CommandTMatchWindowAtTop=1


" vim:fdm=marker foldlevel=0

" Settings -------------------------------------------------------------

" Preamble {{{

" Syntax highlighting {{{
set t_Co=256
set background=dark
if has('win32') || has ('win64')
  let $VIMHOME = expand('~/AppData/Local/nvim/')
else
  let $VIMHOME = expand('~/.config/nvim/')
endif

if filereadable($VIMHOME . 'colors/molotov.vim')
  colorscheme molotov
end
" }}}

" Mapleader {{{
let mapleader=","
" }}}

" Local directories {{{
set backupdir=~/.local/share/nvim/backup
set directory=~/.local/share/nvim/swap
set undodir=~/.local/share/nvim/undo
" }}}

" Set some junk {{{
set completeopt-=preview " Disable scratch buffer for completion preview
set cursorline " Highlight current line
set diffopt=filler " Add vertical spaces to keep right and left aligned
set diffopt+=iwhite " Ignore whitespace changes (focus on code changes)
set encoding=utf-8 nobomb " BOM often causes trouble
set expandtab " Expand tabs to spaces
set fillchars+=vert:\ 
set foldcolumn=0 " Column to show folds
set foldenable " Enable folding
set foldlevel=5 " Open all folds by default
set foldmethod=syntax " Syntax are used to specify folds
set foldminlines=0 " Allow folding single lines
set foldnestmax=5 " Set max fold nesting level
set formatoptions=
set formatoptions+=c " Format comments
set formatoptions+=r " Continue comments by default
set formatoptions+=o " Make comment when using o or O from comment line
set formatoptions+=q " Format comments with gq
set formatoptions+=n " Recognize numbered lists
set formatoptions+=2 " Use indent from 2nd line of a paragraph
set formatoptions+=l " Don't break lines that are already long
set formatoptions+=1 " Break before 1-letter words
set gdefault " By default add g flag to search/replace. Add g to toggle
set hidden " When a buffer is brought to foreground, remember undo history and marks
set ignorecase " Ignore case of searches
set lispwords+=defroutes " Compojure
set lispwords+=defpartial,defpage " Noir core
set lispwords+=defaction,deffilter,defview,defsection " Ciste core
set lispwords+=describe,it " Speclj TDD/BDD
set magic " Enable extended regexes
set mouse=a " Enable the mouse
set noerrorbells " Disable error bells
set nojoinspaces " Only insert single space after a '.', '?' and '!' with a join command
set noshowmode " Don't show the current mode (lightline.vim takes care of us)
set nostartofline " Don't reset cursor to start of line when moving around
set nowrap " Do not wrap lines
set nu " Enable line numbers
set ofu=syntaxcomplete#Complete " Set omni-completion method
set report=0 " Show all changes
set ruler " Show the cursor position
set scrolloff=3 " Start scrolling three lines before horizontal border of window
set shiftwidth=2 " The # of spaces for indenting
set shortmess=atI " Don't show the intro message when starting vim
set sidescrolloff=3 " Start scrolling three columns before vertical border of window
set smartcase " Ignore 'ignorecase' if search patter contains uppercase characters
set softtabstop=2 " Tab key results in 2 spaces
set splitbelow " New window goes below
set splitright " New windows goes right
set suffixes=.bak,~,.swp,.swo,.o,.d,.info,.aux,.log,.dvi,.pdf,.bin,.bbl,.blg,.brf,.cb,.dmg,.exe,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.pyd,.dll
set switchbuf=""
set title " Show the filename in the window titlebar
set termguicolors " Enable true color support
set undofile " Persistent Undo
set viminfo='9999,s512,h " Restore marks are remembered for 9999 files, registers up to 512Kb are remembered, disable hlsearch on start
set visualbell " Use visual bell instead of audible bell (annnnnoying)
set wildchar=<TAB> " Character for CLI expansion (TAB-completion)
set wildignore+=.DS_Store
set wildignore+=*.jpg,*.jpeg,*.gif,*.png,*.gif,*.psd,*.o,*.obj,*.min.js
set wildignore+=*/bower_components/*,*/node_modules/*
set wildignore+=*/smarty/*,*/vendor/*,*/.git/*,*/.hg/*,*/.svn/*,*/.sass-cache/*,*/log/*,*/tmp/*,*/build/*,*/ckeditor/*,*/doc/*,*/source_maps/*,*/dist/*
set winminheight=0 " Allow splits to be reduced to a single line
set wrapscan " Searches wrap around end of file
" }}}

" }}}


" Configuration -------------------------------------------------------------

" General {{{
augroup general_config
  autocmd!

  " Speed up viewport scrolling {{{
  nnoremap <C-e> 3<C-e>
  nnoremap <C-y> 3<C-y>
  " }}}

  " Faster split resizing (+,-) {{{
  if bufwinnr(1)
    map + <C-W>+
    map - <C-W>-
  endif
  " }}}

  " Better split switching (Ctrl-j, Ctrl-k, Ctrl-h, Ctrl-l) {{{
  tnoremap <C-h> <C-\><C-n><C-w>h
  tnoremap <C-j> <C-\><C-n><C-w>j
  tnoremap <C-k> <C-\><C-n><C-w>k
  tnoremap <C-l> <C-\><C-n><C-w>l
  nnoremap <C-j> <C-W>j
  nnoremap <C-k> <C-W>k
  nnoremap <C-h> <C-W>h
  nnoremap <C-l> <C-W>l
  " }}}

  " Sudo write (,W) {{{
  noremap <leader>W :w !sudo tee %<CR>
  " }}}

  " Remap :W to :w {{{
  command! W w
  " }}}

  " Remap : to ; and vice versa {{{
  set langmap=\\;:,:\\;
  " }}}

  " Better mark jumping (line + col) {{{
  nnoremap ' `
  " }}}

  " Hard to type things {{{
  iabbrev >> →
  iabbrev << ←
  iabbrev ^^ ↑
  iabbrev VV ↓
  iabbrev aa λ
  " }}}

  " Toggle show tabs and trailing spaces (,c) {{{
  set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_
  set fcs=fold:-
  nnoremap <silent> <leader>c :set nolist!<CR>
  " }}}

  " Clear last search (,qs) {{{
  map <silent> <leader>qs <Esc>:noh<CR>
  " }}}

  " Remap keys for auto-completion menu {{{
  inoremap <expr> <CR>   pumvisible() ? "\<C-y>" : "\<CR>"
  inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"
  inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"
  " }}}

  " Paste toggle (,p) {{{
  set pastetoggle=<leader>p
  map <leader>p :set invpaste paste?<CR>
  " }}}

  " Yank from cursor to end of line {{{
  nnoremap Y y$
  " }}}

  " Insert newline {{{
  map <leader><Enter> o<ESC>
  " }}}

  " Search and replace word under cursor (,*) {{{
  nnoremap <leader>* :%s/\<<C-r><C-w>\>//<Left>
  vnoremap <leader>* "hy:%s/\V<C-r>h//<left>
  " }}}

  " Strip trailing whitespace (,ss) {{{
  function! StripWhitespace () " {{{
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    :%s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
  endfunction " }}}
  noremap <leader>ss :call StripWhitespace ()<CR>
  " }}}

  " Join lines and restore cursor location (J) {{{
  nnoremap J mjJ`j
  " }}}

  " Toggle folds (<Space>) {{{
  nnoremap <silent> <space> :exe 'silent! normal! '.((foldclosed('.')>0)? 'zMzx' : 'zc')<CR>
  " }}}

  " Fix page up and down {{{
  map <PageUp> <C-U>
  map <PageDown> <C-D>
  imap <PageUp> <C-O><C-U>
  imap <PageDown> <C-O><C-D>
  " }}}

  " Show syntax highlighting group upder cursor (F10) {{{
  map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
        \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
        \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
  " }}}

  " Relative numbers {{{
  set relativenumber " Use relative line numbers. Current line is still in status bar.
  au BufReadPost,BufNewFile * set relativenumber
  " }}}

  " Window title {{{
  set titlestring=%{expand(\"%:t\")} " Set to just the filename
  " }}}
augroup END
" }}}

" Buffers {{{
augroup buffer_control
  autocmd!

  " Buffer navigation (,,) (gb) (gB) (,ls) {{{
  map <Leader>, <C-^>
  map <Leader>ls :buffers<CR>
  map gb :bnext<CR>
  map gB :bprev<CR>
  " }}}

  " Jump to buffer number (<N>gb) {{{
  let c = 1
  while c <= 99
    execute "nnoremap " . c . "gb :" . c . "b\<CR>"
    let c += 1
  endwhile
  " }}}

  " Close Quickfix window (,qq) {{{
  map <leader>qq :cclose<CR>
  " }}}

  " Rename buffer (:Rename) {{{
  function! s:RenameBuffer(name)
    silent! execute 'saveas! ' . a:name
    let l:old_buffer = bufnr("#")
    let l:old_filename = expand("#:t")
    let l:new_buffer = bufnr("%")
    let l:new_filename = expand("%:t")
    silent! execute '!rm ' . shellescape(expand("#"), 1)
    silent! execute 'bd' l:old_buffer
    echom 'Renamed `' . l:old_filename . '` to `' . l:new_filename . '`'
  endfunction
  command! -nargs=1 Rename call s:RenameBuffer(<f-args>)
  " }}}
augroup END
" }}}

" Jumping to tags {{{
augroup jump_to_tags
  autocmd!

  " Basically, <c-]> jumps to tags (like normal) and <c-\> opens the tag in a new
  " split instead.
  "
  " Both of them will align the destination line to the upper middle part of the
  " screen.  Both will pulse the cursor line so you can see where the hell you
  " are.  <c-\> will also fold everything in the buffer and then unfold just
  " enough for you to see the destination line.
  nnoremap <c-]> <c-]>mzzvzz15<c-e>`z:Pulse<cr>
  nnoremap <c-\> <c-w>v<c-]>mzzMzvzz15<c-e>`z:Pulse<cr>

  " Pulse Line (thanks Steve Losh)
  function! s:Pulse() " {{{
    redir => old_hi
      silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    let steps = 8
    let width = 1
    let start = width
    let end = steps * width
    let color = 233

    for i in range(start, end, width)
      execute "hi CursorLine ctermbg=" . (color + i)
      redraw
      sleep 6m
    endfor
    for i in range(end, start, -1 * width)
      execute "hi CursorLine ctermbg=" . (color + i)
      redraw
      sleep 6m
    endfor

    execute 'hi ' . old_hi
  endfunction " }}}
  command! -nargs=0 Pulse call s:Pulse()
augroup END
" }}}

" Highlight Interesting Words {{{
augroup highlight_interesting_word
  autocmd!
  " This mini-plugin provides a few mappings for highlighting words temporarily.
  "
  " Sometimes you're looking at a hairy piece of code and would like a certain
  " word or two to stand out temporarily.  You can search for it, but that only
  " gives you one color of highlighting.  Now you can use <leader>N where N is
  " a number from 1-6 to highlight the current word in a specific color.
  function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
  endfunction " }}}

  " Mappings {{{
  nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
  nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
  nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
  nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
  nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
  nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>
  " }}}

  " Default Highlights {{{
  hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
  hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
  hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
  hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
  hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
  hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195
  " }}}
augroup END
" }}}

" Word Processor Mode {{{
augroup word_processor_mode
  autocmd!

  function! WordProcessorMode() " {{{
    setlocal formatoptions=t1
    map j gj
    map k gk
    setlocal smartindent
    setlocal spell spelllang=en_ca
    setlocal noexpandtab
    setlocal wrap
    setlocal linebreak
    Goyo 100
  endfunction " }}}
  com! WP call WordProcessorMode()
augroup END
" }}}

" Restore Cursor Position {{{
augroup restore_cursor
  autocmd!

  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
augroup END
" }}}


" Filetypes -------------------------------------------------------------

" C {{{
augroup filetype_c
  autocmd!

  " Highlight Custom C Types {{{
  autocmd BufRead,BufNewFile *.[ch] let fname = expand('<afile>:p:h') . '/types.vim'
  autocmd BufRead,BufNewFile *.[ch] if filereadable(fname)
  autocmd BufRead,BufNewFile *.[ch]   exe 'so ' . fname
  autocmd BufRead,BufNewFile *.[ch] endif
  " }}}
augroup END
" }}}

" Clojure {{{
augroup filetype_clojure
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END
" }}}

" JavaScript {{{
augroup filetype_javascript
  autocmd!
  let g:jsx_ext_required = 0
  let g:javascript_plugin_flow = 1
  let g:javascript_plugin_jsdoc = 1
augroup END
" }}}

" Markdown {{{
augroup filetype_markdown
  autocmd!
  let g:markdown_fenced_languages = ['ruby', 'html', 'javascript', 'css', 'erb=eruby.html', 'bash=sh']
augroup END
" }}}

" Ruby {{{
augroup filetype_ruby
  autocmd!

  au BufRead,BufNewFile Rakefile,Capfile,Gemfile,.autotest,.irbrc,*.treetop,*.tt set ft=ruby syntax=ruby

  " Ruby.vim
  let ruby_operators = 1
  let ruby_space_errors = 1
  let ruby_fold = 1
  let g:ruby_indent_block_style = 'do'
  let g:ruby_indent_assignment_style = 'variable'
augroup END
" }}}

" Plugin Configuration -------------------------------------------------------------

" Ale.vim {{{
augroup ale_config
  let g:ale_linters = {'rust': ['rls']}
augroup END
" }}}

" coc.vim {{{
augroup coc_config
  function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
  endfunction

  nmap <leader>d <Plug>(coc-definition)
  nmap <leader>= <Plug>(coc-format-selected)
  vmap <leader>= <Plug>(coc-format-selected)
  nmap <leader>r <Plug>(coc-rename)
  nmap <leader>y <Plug>(coc-codelens-action)
  nmap <leader>x <Plug>(coc-fix-current)
  nmap <leader>z <Plug>(coc-codeaction)

  command! -nargs=0 Format :call CocAction('format')
augroup END
" }}}

" EasyAlign.vim {{{
augroup easy_align_config
  autocmd!
  " Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
  vmap <Enter> <Plug>(EasyAlign)
  " Start interactive EasyAlign for a motion/text object (e.g. <Leader>aip)
  nmap <Leader>a <Plug>(EasyAlign)
augroup END
" }}}

" fzf {{{
augroup fzf_config
  set rtp+=/usr/local/opt/fzf

  let g:fzf_layout = { 'up': '~40%' }
  let g:fzf_history_dir = '~/.config/nvim/fzf-history'
  let g:fzf_buffers_jump = 1 " Jump to existing buffer if available

  if !exists("g:gui_oni")
    " Basic mappings
    nnoremap <C-p> :Files<CR>
    nnoremap <C-g> :GFiles?<CR>
    nnoremap <C-b> :Buffers<CR>
    nnoremap <C-t> :Tags<CR>
    nnoremap <C-m> :Marks<CR>
    nnoremap <leader>l :Lines<CR>

    " Mapping selecting mappings
    nmap <leader><tab> <plug>(fzf-maps-n)
    xmap <leader><tab> <plug>(fzf-maps-x)
    omap <leader><tab> <plug>(fzf-maps-o)

    " Insert mode completion
    imap <c-x><c-k> <plug>(fzf-complete-word)
    imap <c-x><c-f> <plug>(fzf-complete-path)
    imap <c-x><c-j> <plug>(fzf-complete-file-ag)
    imap <c-x><c-l> <plug>(fzf-complete-line)
  end
augroup END
" }}}

" lightline.vim {{{
augroup lightline_config
  autocmd!
  let g:lightline#ale#indicator_checking = "\uf110 "
  let g:lightline#ale#indicator_warnings = "\uf071 "
  let g:lightline#ale#indicator_errors = "\uf05e "
  let g:lightline#ale#indicator_ok = "\uf00c "

  let g:lightline = {}
  let g:lightline.colorscheme = 'material'
  let g:lightline.component_expand = {
        \   'linter_checking': 'lightline#ale#checking',
        \   'linter_warnings': 'lightline#ale#warnings',
        \   'linter_errors': 'lightline#ale#errors',
        \   'linter_ok': 'lightline#ale#ok',
        \ }
  let g:lightline.component_function = {
        \   'cocstatus': 'coc#status',
        \   'currentfunction': 'CocCurrentFunction'
        \ }
  let g:lightline.component_type = {
        \   'linter_checking': 'left',
        \   'linter_warnings': 'warning',
        \   'linter_errors': 'error',
        \   'linter_ok': 'left',
        \ }
  let g:lightline.active = {
        \   'left': [[ 'mode', 'paste' ],
        \            [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ]],
        \   'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ]]
        \ }
augroup END
" }}}

" vim-repeat.vim {{{
augroup repeat_config
  autocmd!
  nmap z <Plug>(RepeatUndo)
  nmap gz <Plug>(RepeatUndoLine)
  nmap Z <Plug>(RepeatRedo)
augroup END
" }}}

" vim-smooth-scroll {{{
augroup vim_smooth_scroll_config
  autocmd!
  noremap <silent> <C-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
  noremap <silent> <C-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
augroup END
" }}}

" Plugins -------------------------------------------------------------

" Load plugins {{{
call plug#begin($VIMHOME . 'plugged')

Plug 'Shougo/neopairs.vim'
Plug 'ap/vim-css-color',         { 'for': 'css' }
Plug 'icatalina/vim-case-change'
Plug 'jooize/vim-colemak'
Plug 'jparise/vim-graphql'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-syntax-extra'
Plug 'lambdalisue/gina.vim'
Plug 'machakann/vim-highlightedyank'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

" Terminal-only plugins
if !exists('g:gui_oni')
  Plug 'itchyny/lightline.vim'
  Plug 'junegunn/fzf',           { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'maximbaz/lightline-ale'
  Plug 'neoclide/coc.nvim',      { 'tag': '*', 'do': './install.sh' }
  Plug 'terryma/vim-smooth-scroll'
  Plug 'tpope/vim-commentary'
  Plug 'w0rp/ale'
  Plug 'wellle/targets.vim'
end

call plug#end()
" }}}

" If using Oni's externalized statusline, hide vim's native statusline,
if exists("g:gui_oni")
  set noruler
  set laststatus=0
  set noshowcmd
endif

" Reload vim-colemak to remap any overridden keys
silent! source "~/.config/nvim/plugged/vim-colemak/plugin/colemak.vim"

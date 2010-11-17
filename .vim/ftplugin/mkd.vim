" mkd.vim
"
" Inspired by textile.vim from Tim Harper (tim.theenchanter.com)
" Heavily modified by Gianni Chiappetta (gf3.ca)

command! -nargs=0 MarkdownRenderFile call MarkdownRenderBufferToFile()
command! -nargs=0 MarkdownRenderTab call MarkdownRenderBufferToTab()
command! -nargs=0 MarkdownPreview call MarkdownRenderBufferToPreview()
noremap <buffer> <Leader>mp :MarkdownPreview<CR>
noremap <buffer> <Leader>mf :MarkdownRenderFile<CR>
noremap <buffer> <Leader>mt :MarkdownRenderTab<CR>
setlocal ignorecase
setlocal wrap
setlocal lbr

function! MarkdownRender(lines)
  if (system('which ruby') == "")
    throw "Could not find ruby!"
  end

  let text = join(a:lines, "\n")
  let html = system("ruby -e \"
        \ require 'rubygems';
        \ require 'maruku';
        \ puts Maruku.new(\\$stdin.read).to_html
        \ \"", text)
  return html
endfunction

function! MarkdownRenderFile(lines, filename)
  " Assumed tpope's pathogen is installed.
  let css = "<link rel=\"stylesheet\" media=\"screen\" href=\"" . pathogen#split(&rtp)[0] . "/ftplugin/mkd-preview.css\" />"
  let html = MarkdownRender(getbufline(bufname("%"), 1, '$'))
  let html = "<!DOCTYPE html><html><head><title>" . bufname("%") . "</title>" . css . "</head><body>\n" . html . "\n</body></html>"
  return writefile(split(html, "\n"), a:filename)
endfunction

function! MarkdownRenderBufferToPreview()
  let filename = "/tmp/markdown-preview.html"
  call MarkdownRenderFile(getbufline(bufname("%"), 1, '$'), filename)

  " Modify this line to make it compatible on other platforms
  call system("open ". filename)
endfunction

function! MarkdownRenderBufferToFile()
  let filename = input("Filename:", substitute(bufname("%"), "markdown$", "html", ""), "file")
  call MarkdownRenderFile(getbufline(bufname("%"), 1, '$'), filename)
  echo "Rendered to '" . filename . "'"
endfunction

function! MarkdownRenderBufferToTab()
  let html = MarkdownRender(getbufline(bufname("%"), 1, '$'))
  tabnew
  call append("^", split(html, "\n"))
  set syntax=html
endfunction


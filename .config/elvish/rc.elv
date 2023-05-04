# Shell stuff
set E:EDITOR = "emacs -nw"
set E:GOPATH = /Users/gianni/.go
set E:LANG = en_CA.UTF-8
set E:LC_ALL = en_US.UTF-8
set E:LESS = "-i -R"
set E:VISUAL = "emacs"

fn have-external { |prog|
  put ?(which $prog >/dev/null 2>&1)
}
fn only-when-external { |prog lambda|
  if (have-external $prog) { $lambda }
}

# Paths
use path

var optpaths = [
  ~/.bin
  ~/.cabal/bin
  ~/.cargo/bin
  $E:GOPATH/bin
  /Applications/Postgres.app/Contents/Versions/15/bin
  /opt/homebrew/bin
  /usr/local/bin
  /usr/bin
  /bin
  /usr/sbin
  /sbin
]

set paths = [(each {|p|
  if (path:is-dir $p) { put $p }
} $optpaths)]

# Direnv
use direnv

# Packages
use epm

epm:install &silent-if-installed         ^
  github.com/zzamboni/elvish-completions ^
  github.com/zzamboni/elvish-modules     ^
  github.com/zzamboni/elvish-themes

# Aliases
use github.com/zzamboni/elvish-modules/alias

alias:new &save .. cd ..
alias:new &save ... cd ../..
alias:new &save .... cd ../../..

only-when-external bat {
  alias:new &save b bat
  alias:new &save more bat --paging always
  set E:MANPAGER = "sh -c 'col -bx | bat -l man -p'"
}

only-when-external zoxide {
  eval (zoxide init elvish | slurp)
}

only-when-external exa {
  alias:new &save l exa --icons -1
  alias:new &save ll exa --icons --long --header --group --created --modified --git
}

# Terminal title
use github.com/zzamboni/elvish-modules/terminal-title

# Chain theme
use github.com/zzamboni/elvish-themes/chain

chain:init
set chain:show-last-chain = $false
set chain:glyph[arrow] = "→"
set chain:glyph[git-branch] = ""
set chain:glyph[git-dirty] = "∴"
set chain:glyph[git-staged] = "★"
set edit:prompt-stale-transform = { each [x]{ styled x[text] "bright-black" } }
set chain:prompt-segment-delimiters = "  "

# Completions
# fn g {|@a| e:git $@a }
# edit:completion:arg-completer[g] = git
set edit:command-abbr['g'] = 'git'
use github.com/zzamboni/elvish-completions/git git-completions
git-completions:init

# Command completion
only-when-external carapace {
 eval (carapace _carapace|slurp)
}

# FZF
set-env FZF_DEFAULT_OPTS '--height 40% --layout=reverse --border'

only-when-external fd {
  set-env FZF_DEFAULT_COMMAND 'fd --type f --strip-cwd-prefix'
}
use asdf _asdf; var asdf~ = $_asdf:asdf~

use asdf _asdf; var asdf~ = $_asdf:asdf~

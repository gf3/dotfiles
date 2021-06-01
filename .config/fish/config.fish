set fish_greeting

# fisher add fishpkg/fish-prompt-mono

set -x COMPOSE_DOCKER_CLI_BUILD 1
set -x DOCKER_BUILDKIT 1
set -x EDITOR "nvim"
set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -x FZF_DEFAULT_COMMAND 'git ls-tree -r --name-only HEAD 2> /dev/null; or fd --type f --hidden --follow --exclude .git 2> /dev/null'
set -x FZF_LEGACY_KEYBINDINGS 0
set -x GOPATH ~/.go
set -x GPG_TTY (tty)
set -x GREP_COLOR "1;37;45"
set -x LANG en_CA.UTF-8
set -x LC_ALL en_CA.UTF-8
set -x RIPGREP_CONFIG_PATH ~/.config/ripgrep/config
set -x RUST_BACKTRACE 1

# Paths
test -d /usr/local/share/npm/bin             ; and set PATH /usr/local/share/npm/bin $PATH
test -d /usr/local/heroku/bin                ; and set PATH /usr/local/heroku/bin $PATH
test -d /usr/local/go/bin                    ; and set PATH /usr/local/go/bin $PATH
test -d /usr/local/sbin                      ; and set PATH /usr/local/sbin $PATH
test -d /usr/local/bin                       ; and set PATH /usr/local/bin $PATH
test -d ~/.bin                               ; and set PATH ~/.bin $PATH
test -d ~/.cabal/bin                         ; and set PATH ~/.cabal/bin $PATH
test -d ~/.cargo/bin                         ; and set PATH ~/.cargo/bin $PATH
test -d ~/.local/bin                         ; and set PATH ~/.local/bin $PATH
test -d $GOPATH/bin                          ; and set PATH $GOPATH/bin $PATH

# Navigation
function ..    ; cd .. ; end
function ...   ; cd ../.. ; end
function ....  ; cd ../../.. ; end
function ..... ; cd ../../../.. ; end

# Utilities
function a        ; command rg --ignore=.git --ignore=log --ignore=tags --ignore=tmp --ignore=vendor --ignore=spec/vcr $argv ; end
function d        ; du -h -d=1 $argv ; end
function df       ; command df -h $argv ; end
function digga    ; command dig +nocmd $argv[1] any +multiline +noall +answer; end
function g        ; git $argv ; end
function grep     ; command grep --color=auto $argv ; end
function httpdump ; sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E "Host\: .*|GET \/.*" ; end
function ip       ; curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g' ; end
function ks       ; command kak-shell $argv ; end
function localip  ; ipconfig getifaddr en0 ; end
function lookbusy ; cat /dev/urandom | hexdump -C | grep --color "ca fe" ; end
function ls       ; command lsd $argv ; end
function tmux     ; command tmux -2 $argv ; end
function tunnel   ; ssh -D 8080 -C -N $argv ; end

alias ssh="kitty +kitten ssh"
alias icat="kitty +kitten icat"

# Gruvbox command line colors
set -x fish_color_command 689d6a
set -x fish_color_param 83a598
set -x fish_color_quote d79921

# Fuzzy find & edit
function vp
  if test (count $argv) -gt 0
    if set -q KAKOUNE_SESSION
      command kak -c $KAKOUNE_SESSION $argv
    else
      command $EDITOR $argv
    end
  else
    fzf -m | xargs $EDITOR
  end
end

# View files/dirs
function c
  if test (count $argv) -eq 0
    if type -q lsd
      lsd --icon always --icon-theme fancy -l
    else if type -q tree
      tree --dirsfirst -aFCNL 1 ./
    else
      ls -l
    end

    return
  end

  for i in $argv
    set_color yellow
    if test (count $argv) -gt 1; echo "$i:" 1>&2; end
    set_color normal

    if test -e $i; and test -r $i
      if test -d $i
        if type -q lsd
          lsd --icon always --icon-theme fancy -l $i
        else if type -q tree
          tree --dirsfirst -aFCNL 1 ./
        else
          ls -l $i
        end
      else if file -b --mime-type $i | string match -q -r '^image\/'
        imgcat $i
      else
        bat --paging never $i
      end
    else
      set_color red
      echo "Cannot open: $i" 1>&2
    end

    set_color normal
  end
end

function l; c $argv; end

function ll
  if type -q lsd
    lsd --icon always --icon-theme fancy -Al $argv
  else if type -q tree
    tree --dirsfirst -aFCNL 1 ./
  else
    ls -l $argv
  end
end

# Completions
function make_completion --argument-names alias command
    echo "
    function __alias_completion_$alias
        set -l cmd (commandline -o)
        set -e cmd[1]
        complete -C\"$command \$cmd\"
    end
    " | source
    complete -c $alias -a "(__alias_completion_$alias)"
end

make_completion g 'git'
make_completion vp 'kak'

# fisher
set fisher_home ~/.local/share/fisherman
if test -f $fisher_home/config.fish
  set fisher_config ~/.config/fisherman
  source $fisher_home/config.fish
end

if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
  curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fish -c fisher
end

# rbenv
if type -q rbenv
  status --is-interactive; and source (rbenv init -|psub)
end

# hub
if type -q hub
  eval (hub alias -s)
end

# iterm2
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

# fnm
if type -q fnm
  fnm env --shell fish --use-on-cd | source
  fnm completions --shell fish | source
end

# brew
if type -q brew
  if test -d (brew --prefix)"/share/fish/completions"
    set -gx fish_complete_path $fish_complete_path (brew --prefix)/share/fish/completions
  end

  if test -d (brew --prefix)"/share/fish/vendor_completions.d"
    set -gx fish_complete_path $fish_complete_path (brew --prefix)/share/fish/vendor_completions.d
  end
end


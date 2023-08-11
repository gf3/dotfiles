# Packages
ZNAP_DIR=~/Code/github.com/marlonrichert/zsh-snap
if [ ! -f $ZNAP_DIR/znap.zsh ]; then
  mkdir -p $ZNAP_DIR &&
  git clone --depth 1 -- \
      https://github.com/marlonrichert/zsh-snap.git $ZNAP_DIR
fi
source "${ZNAP_DIR}/znap.zsh"

# Autocomplete
znap source marlonrichert/zsh-autocomplete

# Path
typeset -U path PATH

path+=(~/.bin)
path+=(~/.cabal/bin)
path+=(~/.cargo/bin)
path+=(~/.fly/bin)
path+=(~/.local/bin)
path+=(~/.asdf/installs/rust/nightly/bin)

function in_path {
  builtin whence -p "$1" &> /dev/null
}

# Environment
export PATH
export EDITOR="hx"
export GOPATH=~/.go
export GPG_TTY=$(tty)
export LANG=en_CA.UTF-8
export LC_ALL=en_US.UTF-8
export LESS="-i -R"
export FLYCTL_INSTALL="/home/gianni/.fly"

# Aliases
alias g=git

if (in_path "bat"); then
  alias b="bat"
  alias more="bat --paging always"
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

if (in_path "exa"); then
  alias l="exa --icons -1"
  alias ls="exa --icons -1"
  alias ll="exa --icons --long --header --group --created --modified --git -a"
fi

if (in_path "systemctl"); then
  systemctl --user import-environment PATH
fi

# Use powerline
USE_POWERLINE="true"
# Source manjaro-zsh-configuration
if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi
# Use manjaro zsh prompt
if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
  source /usr/share/zsh/manjaro-zsh-prompt
fi

# Direnv
if (in_path "direnv"); then
  eval "$(direnv hook zsh)"
fi

# ASDF
if [[ -e ~/.asdf/asdf.sh ]]; then
  . "$HOME/.asdf/asdf.sh"
  fpath=(${ASDF_DIR}/completions $fpath)
fi

# Zoxide
if (in_path "zoxide"); then
  eval "$(zoxide init zsh)"
fi

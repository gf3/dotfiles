# Abbreviations
abbr --add g git
abbr --add b bat

# Aliases
if type -q eza
    alias l="eza --icons --git"
    alias ll="eza --icons --long --header --group --created --modified --git -a"
    alias ls="eza --icons --git"
end

# Environment
set -e SSH_ASKPASS
set -x EDITOR "emacs -nw"
set -x ELIXIR_ERL_OPTIONS "-kernel shell_history enabled"
set -x GOPATH (realpath ~/.go)
set -x GPG_TTY (tty)
set -x LANG "en_CA.UTF-8"
set -x LC_ALL "en_US.UTF-8"
set -x LESS "-i -R"
set -x SSH_AUTH_SOCK (realpath ~/.bitwarden-ssh-agent.sock)

# Path
fish_add_path -m /bin
fish_add_path -m /usr/bin
fish_add_path ~/.bin
fish_add_path ~/.go/bin
fish_add_path -m ~/.local/bin

# Direnv
if type -q direnv
    direnv hook fish | source
end

# Systemctl
if type -q systemctl
    systemctl --user import-environment PATH
end

# Zoxide
if type -q zoxide
    zoxide init fish | source
end

# Cargo
if type -q cargo
    source "$HOME/.cargo/env.fish"
end

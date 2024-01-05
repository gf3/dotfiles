# Prompt
set --universal tide_left_prompt_items context pwd git character

# Abbreviations
abbr --add g git
abbr --add b bat

# Aliases
if type -q exa
    alias l="exa -l"
    alias ll="exa --icons --long --header --group --created --modified --git -a"
    alias ls="exa -l"
end

# Environment
set -x EDITOR "hx"
set -x ELIXIR_ERL_OPTIONS "-kernel shell_history enabled"
set -x GOPATH (realpath ~/.go)
set -x GPG_TTY (tty)
set -x LANG "en_CA.UTF-8"
set -x LC_ALL "en_US.UTF-8"
set -x LESS "-i -R"
set -x FLYCTL_INSTALL "/home/gianni/.fly"

# ASDF
if test -e ~/.asdf/asdf.fish
    # Manually add asdf dirs to path, to avoid having them prepended
    fish_add_path -aP ~/.asdf/bin
    fish_add_path -aP ~/.asdf/shims

    source ~/.asdf/asdf.fish
    fish_add_path ~/.asdf/installs/rust/nightly/bin

    if ! test -e ~/.config/fish/completions/asdf.fish
        mkdir -p ~/.config/fish/completions; and ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions
    end
end

if test -e ~/.asdf/plugins/golang/set-env.fish
    source ~/.asdf/plugins/golang/set-env.fish
end

# Path
fish_add_path -m /bin
fish_add_path -m /usr/bin
fish_add_path ~/.bin
fish_add_path ~/.cabal/bin
fish_add_path ~/.cargo/bin
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

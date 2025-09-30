status is-login; and begin
# Login shell initialisation
end

status is-interactive; and begin
# Abbreviations
abbr --add -- b bat
abbr --add -- g git

# Aliases
alias ... 'cd ../..'
alias l 'eza --icons --git'
alias ll 'eza --icons --long --header --group --created --modified --git -a'

# Interactive shell initialisation
set -gx GPG_TTY (tty)
set -gx EDITOR "emacs -nw"
set -gx ELIXIR_ERL_OPTIONS "-kernel shell_history enabled"
# set -gx SSH_AUTH_SOCK "~/.bitwarden-ssh-agent.sock"
set -gx SSH_AUTH_SOCK ~/.var/app/com.bitwarden.desktop/data/.bitwarden-ssh-agent.sock

# Tools
direnv hook fish | source
zoxide init fish | source
end
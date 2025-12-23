status is-login; and begin
    # Login shell initialisation
end

status is-interactive; and begin
    # Abbreviations
    abbr --add -- b bat
    abbr --add -- g git
    abbr --add -- k kubectl

    # Aliases
    alias ... 'cd ../..'
    alias .... 'cd ../../..'
    alias ..... 'cd ../../../..'
    alias l 'lsd -h --group-dirs=first --git'
    alias ll 'lsd --long -h --group-dirs=first --git -A'

    # Interactive shell initialisation
    set -gx GPG_TTY (tty)
    set -gx EDITOR nano
    set -gx ELIXIR_ERL_OPTIONS "-kernel shell_history enabled"
    # set -gx SSH_AUTH_SOCK "~/.bitwarden-ssh-agent.sock"
    set -gx SSH_AUTH_SOCK ~/.var/app/com.bitwarden.desktop/data/.bitwarden-ssh-agent.sock

    # Tools
    direnv hook fish | source
    zoxide init fish | source
end

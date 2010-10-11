# Aliases
if [ -f ~/.aliases ]; then
  source ~/.aliases
fi

# Prompt
if [ -f ~/.bash_prompt ]; then
  source ~/.bash_prompt
fi

# Editor
export EDITOR="vim"

# Larger bash history (default is 500)
export HISTFILESIZE=10000
export HISTSIZE=10000

# PATH + Private bin
if [ -d ~/.scripts ] ; then
    PATH="~/.scripts:$PATH"
fi
PATH="/usr/local/bin:/usr/local/sbin:$PATH"


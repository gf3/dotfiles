# Prompt
if [ -f ~/.bash_prompt ]; then
  source ~/.bash_prompt
fi

# Larger bash history (default is 500)
export HISTFILESIZE=10000
export HISTSIZE=10000

PATH="/usr/local/bin:/usr/local/sbin:$PATH"

# Some junk
export JAVA_HOME="/Library/Java/Home"
export JRUBY_HOME="/Users/gianni/Projects/java/jruby-1.3.1"

# Common junk
[[ -s "$HOME/.commonrc" ]] && source "$HOME/.commonrc"

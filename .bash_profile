# Aliases
if [ -f ~/.bash_aliases ]; then
  source ~/.bash_aliases
fi

# Prompt
if [ -f ~/.bash_prompt ]; then
  source ~/.bash_prompt
fi

# Editor
export EDITOR="mate -w"

# Larger bash history (default is 500)
export HISTFILESIZE=10000
export HISTSIZE=10000

# PATH + Private bin
if [ -d ~/.scripts ] ; then
    PATH="~/.scripts:$PATH"
fi
PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"

# Some junk
export JAVA_HOME="/Library/Java/Home"
export JRUBY_HOME="/Users/gianni/Projects/java/jruby-1.3.1"

# http://blog.thinkrelevance.com/2009/7/29/ruby-switcher-working-with-multiple-ruby-versions-has-never-been-this-easy/
# source ~/.ruby_switcher.sh

# -- start rip config -- #
RIPDIR=/Users/gianni/.rip
RUBYLIB="$RUBYLIB:$RIPDIR/active/lib"
PATH="$PATH:$RIPDIR/active/bin"
export RIPDIR RUBYLIB PATH
# -- end rip config -- #

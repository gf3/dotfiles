# Misc
alias ll="ls -lahG"
alias ls="ls -G"
alias df="df -h"
alias h="history"
alias g="git"
alias a="ack -ia"
alias grep='GREP_COLOR="1;37;41" LANG=C grep --color=auto'
alias ip="curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g'"
alias localip="ipconfig getifaddr en1"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Gianni
alias sync_irc="rsync -avz ~/Sites/IRC/ slice:/home/gianni/www/irc.gf3.ca"

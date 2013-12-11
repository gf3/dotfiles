set fish_greeting

set -x EDITOR vim
set -x GREP_COLOR "1;37;45"
set -x JRUBYOPT "-Xcext.enabled=true"
set -x LC_ALL en_US.UTF-8  
set -x LANG en_US.UTF-8
set -x RBXOPT -X19

# Paths
test -d /usr/local/share/npm/bin ; and set PATH /usr/local/share/npm/bin $PATH
test -d /usr/local/racket/bin    ; and set PATH /usr/local/racket/bin $PATH
test -d /usr/local/heroku/bin    ; and set PATH /usr/local/heroku/bin $PATH
test -d /usr/local/sbin          ; and set PATH /usr/local/sbin $PATH
test -d /usr/local/bin           ; and set PATH /usr/local/bin $PATH

# Navigation
function ..   ; cd .. ; end
function ...  ; cd ../.. ; end
function .... ; cd ../../.. ; end
function ~    ; cd ~ ; end
function l    ; tree --dirsfirst -aFCNL 1 $argv ; end
function ll   ; tree --dirsfirst -ChFupDaLg 1 $argv ; end

# Utilities
function a        ; command ag $argv ; end 
function b        ; bundle exec $argv ; end 
function c        ; pygmentize -O style=monokai -f console256 -g $argv ; end 
function d        ; du -h -d=1 $argv ; end 
function df       ; command df -h $argv ; end 
function digga    ; command dig +nocmd $argv[1] any +multiline +noall +answer; end
function f        ; foreman run bundle exec $argv ; end
function g        ; git $argv ; end 
function grep     ; command grep --color=auto $argv ; end
function httpdump ; sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E "Host\: .*|GET \/.*" ; end 
function ip       ; curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g' ; end 
function localip  ; ipconfig getifaddr en1 ; end 
function mp       ; mvim -p $argv ; end 
function mutt     ; command bash --login -c 'cd ~/Desktop; /usr/local/bin/mutt' $argv; end
function rkt      ; racket -il xrepl $argv ; end 
function tmux     ; command tmux -2 $argv ; end 
function tunnel   ; ssh -D 8080 -C -N $argv ; end
function view     ; vim -p -R $argv ; end 
function vp       ; vim -p $argv ; end 

function make_completion --argument-names alias command
    echo "
    function __alias_completion_$alias
        set -l cmd (commandline -o)
        set -e cmd[1]
        complete -C\"$command \$cmd\"
    end
    " | .
    complete -c $alias -a "(__alias_completion_$alias)"
end

make_completion b 'bundle exec'
make_completion g 'git'
make_completion mp 'vim -p'
make_completion vp 'vim -p'

# rbenv
begin
  set -lx has_rbenv false

  if test -d $HOME/.rbenv
    set has_rbenv $HOME/.rbenv
  else if test -d /usr/local/rbenv
    set has_rbenv /usr/local/rbenv
  end

  if test $has_rbenv
    set PATH $has_rbenv/bin $PATH
    set PATH $has_rbenv/shims $PATH
    rbenv rehash >/dev/null ^&1
  end
end


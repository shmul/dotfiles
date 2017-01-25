#!/usr/bin/bash

[ -f .host-specific.sh ] && source .host-specific.sh

# from http://www.caliban.org/bash/
#export CDPATH=.:~:/mnt:/mnt/c/dev//mnt/c/dev/trunk/:/usr/src:/usr/lib:/usr/local
export DISPLAY=:0.0
export EDITOR=/usr/bin/vim
export PATH=$PATH:/Developer/usr/bin:~/bin

export JAVA_HOME=/Library/Java/Home
export GOROOT=/opt/local/lib/go
export GOPATH=$HOME/dev/gw
export PATH=$PATH:$GOROOT/bin
export PATH=$PATH:$GOPATH/bin
export HISTIGNORE="&:[bf]g:exit"
export FIGNORE=".svn:"
alias ll="ls -lhog"
alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/ /' -e 's/-/|/'"
alias renew="sudo ipconfig set en6 DHCP"

#alias ssh=~/bin/iterm_bg_image

shopt -s histappend
shopt -s cdspell
shopt -s dotglob
#PROMPT_COMMAND='history -a'

CLICOLOR="YES";    export CLICOLOR

#LSCOLORS="ExGxFxdxCxDxDxhbadExEx";    export LSCOLORS
case $TERM in
    xterm*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}:${PWD}\007"'
        ;;
    *)
        ;;
esac

export SUDO_PS1="\[\e[33;1;41m\][\u] \w \$\[\e[0m\] "
alias tcpd='sudo tcpdump -s 0 -A -i en0 port 80'
mkcdir() { mkdir -p $1; cd $1; }
#[ -f ~/.git-bash-completion.sh ] && . ~/.git-bash-completion.sh
. /opt/local/share/git/contrib/completion/git-completion.bash

. /opt/local/share/git/contrib/completion/git-prompt.sh

# if [ "$TERM" = "linux" ]
# then
#     #we're on the system console or maybe telnetting in
#     export PS1="\[\e[32;1m\]\u@\H > \[\e[0m\]"
# else
#     #we're not on the console, assume an xterm
#  #  export PS1="\[\e]2;\u@\H \w\a\e[32;1m\]>\[\e[0m\] "
#     export PS1='\[\e]0;\w\a\]\[\e[32m\][$(date +%H:%M)] \u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '
# fi
# if [ "\$(type -t __git_ps1)" ]; then
#     PS1="$PS1 \$(__git_ps1 '(%s)')\n"
# fi

export PS1='\[\e]0;\w\a\]\[\e[32m\][$(date +%H:%M)] \u@\h \[\e[33m\]\w\[\e[0m\]$(__git_ps1)\n\$ '

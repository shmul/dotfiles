#!/usr/bin/bash

[ -f .host-specific.sh ] && source .host-specific.sh

# from http://www.caliban.org/bash/
#export CDPATH=.:~:/mnt:/mnt/c/dev//mnt/c/dev/trunk/:/usr/src:/usr/lib:/usr/local
export DISPLAY=:0.0
export EDITOR=/usr/bin/vim
export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/go/bin:$HOME/go/bin:$PATH"
export HISTIGNORE="&:[bf]g:exit"
export FIGNORE=".svn:"
alias ll="ls -lhog"
export LESS='-R'

alias pbs="pushd ../pbs.git; git pull;popd"
#alias ssh=~/bin/iterm_bg_image
alias killjh='pkill -f "ssh jumphost"'

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

. /usr/local/etc/bash_completion.d/git-completion.bash
. /usr/local/etc/bash_completion.d/git-prompt.sh

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

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
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\[\e]0;\w\a\]\[\e[32m\][$(date +%H:%M)] \u@\h \[\e[33m\]\w\[\e[0m\]$(__git_ps1)\n\$ '

export GTAGSLABEL=pygments

alias jh="ssh jumphost"
[ -f ~/local/bin/funcs.sh ] & source ~/local/bin/funcs.sh
[ -f ~/local/bin/bookmarks.sh ] & source ~/local/bin/bookmarks.sh

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export DISPLAY=:0.0
export EDITOR=/usr/bin/vim

export HISTIGNORE="&:[bf]g:exit"
export HISTCONTROL="ignoreboth"
export FIGNORE=".svn:"

# don't share history across buffers/windows/panes
setopt nosharehistory

# https://leetschau.github.io/remove-duplicate-zsh-history.html
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_BEEP

export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/go/bin:$HOME/go/bin:$HOME/.cargo/bin:$PATH"
#if [ -f ~/.bashrc ]; then
#   source ~/.bashrc
#fi

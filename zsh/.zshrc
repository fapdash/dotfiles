#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Also, add the following to your .zshrc to allow emacs to track your current directory as you cd around.
# source: https://stackoverflow.com/a/10050104
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

# shell history for erlang / iex
export ERL_AFLAGS="-kernel shell_history enabled"

twitch() {
    if [ -z $2 ]; then
        quality="best"
    else
        quality=$2
    fi

    livestreamer "twitch.tv/$1" $quality
}

gc() {
    gradle $1 --continuous
}

# use this to rebase master on upstream
default_repo=upstream
function gitup(){
  git stash && git checkout master && git pull ${1-$default_repo} master && git stash pop
}

# use this to rebase non-master branch on upstream
default_repo=upstream
function gitupp(){
  git stash && git checkout master && git pull ${1-$default_repo} master && git checkout - && git rebase master && git stash pop
}

export PATH="/home/fap/.cask/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/fap/.sdkman"
[[ -s "/home/fap/.sdkman/bin/sdkman-init.sh" ]] && source "/home/fap/.sdkman/bin/sdkman-init.sh"

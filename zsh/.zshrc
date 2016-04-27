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

twitch() {
    if [ -z $2 ]; then
        quality="best"
    else
        quality=$2
    fi  

    livestreamer "twitch.tv/$1" $quality
}

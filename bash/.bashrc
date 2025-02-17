# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000000
HISTFILESIZE=2000000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export ERL_AFLAGS="-kernel shell_history enabled"

export EDITOR="emacsclient -nw"

function ec() {
    emacsclient -nw $1
}

function key_layout_switched() {
    xkbcomp -I/home/fap/repos/dotfiles/xkb ~/repos/dotfiles/xkb/keymap/mykbd $DISPLAY 2> /dev/null
}

function key_layout_default() {
    xkbcomp -I/home/fap/repos/dotfiles/xkb ~/repos/dotfiles/xkb/keymap/default $DISPLAY 2> /dev/null
}

function ert-run() {
    emacs -batch -l ert -l $1 -f ert-run-tests-batch-and-exit
}

# Some of the most useful features in emacs-libvterm require shell-side
# configurations. The main goal of these additional functions is to enable the
# shell to send information to `vterm` via properly escaped sequences. A
# function that helps in this task, `vterm_printf`, is defined below.

function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

# This is to change the title of the buffer based on information provided by the
# shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
# various symbols.
PROMPT_COMMAND='echo -ne "\033]0;\h:\w\007"'

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi


# https://superuser.com/a/1356803/669906
snap_delete_old_versions() {
    snap list --all | while read snapname ver rev trk pub notes; do if [[ $notes = *disabled* ]]; then sudo snap remove "$snapname" --revision="$rev"; fi; done
}

server_here() {
    python3 -m http.server --bind 127.0.0.1
}

boop () {
  local last="$?"
  if [[ "$last" == '0' ]]; then
    sfx good
  else
    sfx bad
  fi
  $(exit "$last")
}

function time_func() {
   date2=$((`date +%s` + $1));
   date1=`date +%s`;
   date_finish="$(date --date @$(($date2)) +%T )"

   if [ -n "${2}" ]; then
       COLOR='\033[0;36m'
       NC='\033[0m' # No Color
       echo -e "Timer for: ${COLOR}$2${NC}\n"
   fi
   echo "Start at `date +%T`   Will finish at $date_finish"

    while [ "$date2" -ne `date +%s` ]; do
     echo -ne "     Since start: $(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)     Till end:  $(date -u --date @$(($date2 - `date +%s`)) +%H:%M:%S)\r";
     sleep 1
    done

    echo -ne "     Since start: $(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)     Till end:  $(date -u --date @$(($date2 - `date +%s`)) +%H:%M:%S)\r";
    printf "\nTimer finished!\n"
    sfx clock-alarm
}

function timer_seconds() {
  echo "Counting to $1 seconds"
  time_func $1 $2
}

function timer_minutes() {
  echo "Counting to $1 minutes"
  time_func $1*60 $2
}

function timer_hours() {
  echo "Counting to $1 hours"
  time_func $1*60*60 $2
}

# Accepts flexible input hh:mm:ss
function timer_flexible() {
    echo "Counting to $1"
    secs=$(time2seconds $1)
    time_func $secs $2
}

# Changes hh:mm:ss to seconds. Found in some other Stack Exchange answer
function time2seconds() {
    a=( ${1//:/ })
    echo $((${a[0]}*3600+${a[1]}*60+${a[2]}))
}

function colorpicker() {
# Get the gdbus output
output=$(gdbus call --session --dest org.gnome.Shell.Screenshot --object-path /org/gnome/Shell/Screenshot --method org.gnome.Shell.Screenshot.PickColor)
colors=($(echo $output | command grep -o "[0-9\.]*"))

# Convert to 255-based RGB format
for ((i = 0; i < ${#colors[@]}; i++)); do
    colors[$i]=$(printf '%.0f' $(echo "${colors[$i]} * 255" | bc))
done

echo   "RGB: ${colors[0]} ${colors[1]} ${colors[2]}"
printf "HEX: #%02x%02x%02x\n" "${colors[0]}" "${colors[1]}" "${colors[2]}"
}

# exercism.io
if [ -f ~/.config/exercism/exercism_completion.bash ]; then
  source ~/.config/exercism/exercism_completion.bash
fi

PATH=$PATH:/usr/lib/dart/bin:$HOME/.pub-cache/bin:$HOME/development/flutter/bin:$HOME/development/flutter/.pub-cache/bin
PATH=$PATH:~/.cache/rebar3/bin
PATH=$PATH:~/Android/Sdk/platform-tools
PATH=$PATH:~/j9.5/bin
PATH=$PATH:~/.fzf/bin
PATH=$PATH:~/age
eval "$(thefuck --alias)"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export http_proxy=''
export https_proxy=''
export ftp_proxy=''
export socks_proxy=''
PATH=~/.local/bin:/snap/bin:$PATH

[ -f "/home/fap/.ghcup/env" ] && source "/home/fap/.ghcup/env" # ghcup-env

# pnpm
export PNPM_HOME="/home/fap/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/fap/.sdkman"
[[ -s "/home/fap/.sdkman/bin/sdkman-init.sh" ]] && source "/home/fap/.sdkman/bin/sdkman-init.sh"

eval "$(/home/fap/.local/bin/mise activate bash)"
export PATH=$PATH:/usr/local/go/bin:~/go/bin

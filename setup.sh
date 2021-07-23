#!/usr/bin/env bash


################################
######### Emacs ################
################################
ln -s ~/repos/dotfiles/emacs/.emacs ~/.emacs
mkdir ~/.emacs.d 2> /dev/null
ln -s ~/repos/dotfiles/emacs/lisp/ ~/.emacs.d/lisp


################################
######### streamlink ###########
################################
ln -s ~/repos/dotfiles/streamlink ~/.config/streamlink


################################
######### fzf ##################
################################
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

################################
######### Ruby #################
################################
ln -s ~/repos/dotfiles/ruby/.pryrc ~/.pryrc
ln -s ~/repos/dotfiles/ruby/.irbrc ~/.irbrc

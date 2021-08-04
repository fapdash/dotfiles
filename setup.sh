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


################################
######### Bash #################
################################
ln -s ~/repos/dotfiles/bash/.bashrc ~/.bashrc


################################
######### rbenv ################
################################
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
mkdir -p "$(rbenv root)"/plugins
git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
# Optionally, try to compile dynamic bash extension to speed up rbenv.
# Don't worry if it fails; rbenv will still work normally:
# cd ~/.rbenv && src/configure && make -C src


################################
######### sdkman ###############
################################
curl -s "https://get.sdkman.io?rcupdate=false" | bash


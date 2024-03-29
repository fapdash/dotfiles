#!/usr/bin/env bash


################################
######### Emacs ################
################################
ln -s ~/repos/dotfiles/emacs/.emacs ~/.emacs
mkdir ~/.emacs.d 2> /dev/null
ln -s ~/repos/dotfiles/emacs/lisp/ ~/.emacs.d/lisp
ln -s ~/repos/dotfiles/emacs/.rg_ignore ~/.emacs.d


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

################################
######### ledger ###############
################################
sudo apt install ledger -y
pip3 install ledger-autosync --user
mkdir -p ~/.config/ledger-autosync/plugins
ln -s ~/repos/dotfiles/ledger-autosync/plugins/teo.py ~/.config/ledger-autosync/plugins/teo.py

################################
######### snap #################
################################
# only retain 1 old version of installed snaps
# https://superuser.com/a/1361201/669906
sudo snap set system refresh.retain=2

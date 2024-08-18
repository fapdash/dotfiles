#!/usr/bin/env bash


################################
######### Emacs ################
################################
sudo snap install emacs --classic
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
######### Bash #################
################################
ln -s ~/repos/dotfiles/bash/.bashrc ~/.bashrc


################################
######### mise #################
################################
curl https://mise.run | sh
$HOME/.local/bin/mise activate bash

mise use -g node@latest
mise use -g erlang@latest
mise use -g elixir@latest
mise use -g dart@latest
mise use -g go@latest
mise use -g python@latest

npm install -g pnpm
npm install -g yarn
npm install -g stylelint

pip3 install thefuck --user
pip3 install ansible --user
pip3 install yamllint --user
pip3 install flake8 --user


################################
######### chezmoi###############
################################
./install_chezmoi.sh


################################
######### Ruby #################
################################
mise use -g ruby@latest
ln -s ~/repos/dotfiles/ruby/.pryrc ~/.pryrc
ln -s ~/repos/dotfiles/ruby/.irbrc ~/.irbrc

################################
######### sdkman ###############
################################
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk update

sdk install java 11.0.23-tem
sdk install java 17.0.11-tem
sdk install java 21.0.3-tem
sdk default java 21.0.3-tem
sdk install gradle

################################
######### ledger ###############
################################
sudo apt install ledger -y
pip3 install https://github.com/egh/ledger-autosync/archive/refs/heads/master.tar.gz --user
mkdir -p ~/.config/ledger-autosync/plugins
ln -s ~/repos/dotfiles/ledger-autosync/plugins/teo.py ~/.config/ledger-autosync/plugins/teo.py

################################
######### snap #################
################################
# only retain 1 old version of installed snaps
# https://superuser.com/a/1361201/669906
sudo snap set system refresh.retain=2

# TODO(FAP): vagrant
# TODO(FAP): vscode, vscodium
# TODO(FAP): virtualbox
# TODO(FAP): dart-sass
# TODO(FAP): chezscheme
# TODO(FAP): sbcl + quicklisp
# TODO(FAP): docker: https://docs.docker.com/engine/install/ubuntu/
# TODO(FAP): guile
# TODO(FAP): jq
# TODO(FAP): clojure
#   sdk install leiningen
#   mise use -g clojure@latest
#   mise use -g clj-kondo@latest
# TODO(FAP): postgres
# TODO(FAP): mysql
# TODO(FAP): mariadb
# TODO(FAP): redis
# TODO(FAP): dbeaver
# TODO(FAP): beekeeper-studio
# TODO(FAP): duckdb
# TODO(FAP): wget
# TODO(FAP): webp
# TODO(FAP): wireguard
# TODO(FAP): wireshark
# TODO(FAP): fireshot
# TODO(FAP): shotwell
# TODO(FAP): peek
# TODO(FAP): ctags
# TODO(FAP): build-essential
# TODO(FAP): Zellij
# TODO(FAP): android-sdk
# TODO(FAP): libenchant-2-dev
# TODO(FAP): shellcheck
# TODO(FAP): ag / silversurfer
# TODO(FAP): tldr

# jinx (emacs spell checker) dependency
sudo apt install libenchant-2-dev
sudo apt install ripgrep
if [ ! -d ~/plantuml ]; then
    mkdir ~/plantuml
fi
curl -o ~/plantuml/plantuml.jar https://github.com/plantuml/plantuml/releases/download/v1.2024.6/plantuml-1.2024.6.jar
sudo apt install pandoc

gem install asciidoctor
gem install pry pry-doc
gem install debug
gem install lolcat
gem install sqlint
gem install mdl # markdownlint

sudo apt install cowsay -y


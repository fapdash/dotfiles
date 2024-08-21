#!/usr/bin/env bash

sudo apt install -y git\
                    curl\
                    wget

################################
######### chezmoi###############
################################
./install_chezmoi.sh
chezmoi init https://github.com/fapdash/dotfiles.git
chezmoi apply
ln -s ~/.local/share/chezmoi/ ~/git/dotfiles
# TODO(FAP): change remote of ~/.local/share/chezmoi/ to use ssh

################################
######### Emacs ################
################################
sudo snap install emacs --classic
ln -s ~/git/dotfiles/emacs/.emacs ~/.emacs
mkdir ~/.emacs.d 2> /dev/null
ln -s ~/git/dotfiles/emacs/lisp/ ~/.emacs.d/lisp
ln -s ~/git/dotfiles/emacs/.rg_ignore ~/.emacs.d


################################
######### streamlink ###########
################################
ln -s ~/git/dotfiles/streamlink ~/.config/streamlink


################################
######### fzf ##################
################################
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install


################################
######### Bash #################
################################
mv ~/.bashrc ~/.bashrc.bak
ln -sf ~/git/dotfiles/bash/.bashrc ~/.bashrc
source ~/.bashrc


################################
######### mise #################
################################
curl https://mise.run | sh
$HOME/.local/bin/mise activate bash
# TODO(FAP): activating mise immediately didn't work?

mise use -g node@latest

# https://github.com/asdf-vm/asdf-erlang?tab=readme-ov-file#ubuntu-2404-lts
sudo apt-get -y install build-essential autoconf m4 libncurses5-dev libwxgtk3.2-dev libwxgtk-webview3.2-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop libxml2-utils libncurses-dev
mise use -g erlang@latest

mise use -g elixir@latest
# TODO(FAP): flutter wants dart to come from flutter sdk
mise use -g dart@latest
dart --disable-analytics
sudo apt install libgtk-3-dev ninja-build cmake clang -y

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
sudo apt-get install libz-dev libssl-dev libffi-dev libyaml-dev -y
mise use -g ruby@latest
ln -s ~/git/dotfiles/ruby/.pryrc ~/.pryrc
ln -s ~/git/dotfiles/ruby/.irbrc ~/.irbrc

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
ln -s ~/git/dotfiles/ledger-autosync/plugins/teo.py ~/.config/ledger-autosync/plugins/teo.py
ln -s ~/git/dotfiles/ledger-autosync/plugins/dkb.py ~/.config/ledger-autosync/plugins/dkb.py

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
# TODO(FAP): babashka
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
# TODO(FAP): age / rage
# TODO(FAP): entr

# jinx (emacs spell checker) dependency
sudo apt install libenchant-2-dev -y
if [ ! -d ~/plantuml ]; then
    mkdir ~/plantuml
fi
curl -o ~/plantuml/plantuml.jar https://github.com/plantuml/plantuml/releases/download/v1.2024.6/plantuml-1.2024.6.jar
sudo apt install pandoc -y

gem install asciidoctor
gem install pry pry-doc
gem install debug
gem install lolcat
gem install sqlint
gem install mdl # markdownlint

sudo apt install cowsay -y

sudo apt install nemo -y
xdg-mime default nemo.desktop inode/directory

sudo apt install mg -y

# vterm dependency
sudo apt install libtool-bin -y

sudo apt install w3m -y

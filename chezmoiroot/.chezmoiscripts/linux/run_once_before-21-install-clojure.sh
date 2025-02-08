#!/usr/bin/env bash

set -eufo pipefail

# also needs Java to be installed, need to run this script after installing Java
sudo apt install -y bash curl rlwrap

curl -L -O --output-dir ~/Downloads https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh

chmod +x ~/Downloads/linux-install.sh

sudo ~/Downloads/linux-install.sh

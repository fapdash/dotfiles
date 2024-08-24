#!/usr/bin/env bash

set -eufo pipefail

if [ -f ~/.local/bin/emacs-lsp-booster ]; then
    rm ~/.local/bin/emacs-lsp-booster
fi

lspBoosterVersion=0.2.1
boosterFile=emacs-lsp-booster_v"$lspBoosterVersion"_x86_64-unknown-linux-musl.zip
sha256Expected=f47745e6754eb5a54525da705ba2c83330ba5fbf6613e8ebbe728894fd0b468e

curl -fsSL -O --output-dir ~/Downloads https://github.com/blahgeek/emacs-lsp-booster/releases/download/v"$lspBoosterVersion"/"$boosterFile"

sha256Actual=$(sha256sum ~/Downloads/$boosterFile)
[ ! "${sha256Actual%% *}" = "$sha256Expected" ] &&  echo -e "checksum check failed for $boosterFile!\n${sha256Actual%% *} != $sha256Expected" && exit 1

unzip -nq -d ~/.local/bin ~/Downloads/emacs-lsp-booster_v"$lspBoosterVersion"_x86_64-unknown-linux-musl.zip

rm ~/Downloads/emacs-lsp-booster_v"$lspBoosterVersion"_x86_64-unknown-linux-musl.zip

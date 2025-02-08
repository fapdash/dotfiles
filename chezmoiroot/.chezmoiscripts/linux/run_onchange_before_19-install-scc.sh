#!/usr/bin/env bash

set -eufo pipefail

version="3.3.5"
file="scc_Linux_x86_64.tar.gz"
sha256Expected="d6c00adae232e8949e5426268a17ece9c1cb55b4f628c413bdf018c45ee78cd8"

curl -fsSL -O --output-dir ~/Downloads "https://github.com/boyter/scc/releases/download/v$version/$file"

sha256Actual=$(sha256sum ~/Downloads/$file)
[ ! "${sha256Actual%% *}" = "$sha256Expected" ] &&  echo -e "scc checksum check failed!\n${sha256Actual%% *} != $sha256Expected" && exit 1

tar -xvzf ~/Downloads/"$file" -C ~/.local/bin/ scc

rm ~/Downloads/$file

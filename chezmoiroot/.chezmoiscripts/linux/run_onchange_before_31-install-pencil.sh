#!/usr/bin/env bash

set -eufo pipefail

version="3.1.1"
filename="Pencil_$version.ga_amd64.deb"
sha512Expected="5176cb2a5b855f1317e1a6a58da35190dd015b36b049a050a6e50919e7418db583c11208040a5ec82a41c24e3a2c6510cd4ce8b79e68819128793e822a21fdda"

curl -fsSL -O --output-dir ~/Downloads "https://pencil.evolus.vn/dl/V$version.ga/$filename"

sha512Actual=$(sha512sum ~/Downloads/$filename)
[ ! "${sha512Actual%% *}" = "$sha512Expected" ] &&  echo -e "Pencil checksum check failed!\n${sha512Actual%% *} != $sha512Expected" && exit 1

sudo apt-get install ~/Downloads/$filename -y

rm ~/Downloads/$filename

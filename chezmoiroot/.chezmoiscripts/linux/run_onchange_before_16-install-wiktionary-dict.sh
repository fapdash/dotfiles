#!/usr/bin/env bash

set -eufo pipefail

version="2023-07-27"
filename=wikt-en-en-"$version"-dictd.7z
sha256Expected=294d95eb686f866e3601eb597c5dcb987f8c41acec97a47a6cc51c073eb36d96

curl -fsSL -O --output-dir ~/Downloads https://www.dictinfo.com/dictd/"$filename"

sha256Actual=$(sha256sum ~/Downloads/$filename)
[ ! "${sha256Actual%% *}" = "$sha256Expected" ] &&  echo -e "checksum check failed for $filename!\n${sha256Actual%% *} != $sha256Expected" && exit 1

7z e ~/Downloads/"$filename" -o"$HOME"/Downloads
rm ~/Downloads/"$filename"

sudo mv ~/Downloads/wikt-en-en-"$version".dict.dz /usr/share/dictd/
sudo mv ~/Downloads/wikt-en-en-"$version".index /usr/share/dictd/
rm -rf ~/Downloads/wikt-en-en-"$version"-dictd

sudo /usr/sbin/dictdconfig -w
sudo systemctl reload dictd

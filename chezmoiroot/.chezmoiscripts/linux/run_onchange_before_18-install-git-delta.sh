#!/usr/bin/env bash

set -eufo pipefail

version="0.18.1"
sha256Expected="75481e2d4b2a99ff24d72cf3860aea816a2f33117247df0c1ca70eb58fbf360e"
file=git-delta_"$version"_amd64.deb

curl -fsSL -O --output-dir ~/Downloads https://github.com/dandavison/delta/releases/download/"$version"/"$file"

sha256Actual=$(sha256sum ~/Downloads/"$file")
[ ! "${sha256Actual%% *}" = "$sha256Expected" ] &&  echo -e "delta checksum check failed!\n${sha256Actual%% *} != $sha256Expected" && exit 1

sudo apt install -y ~/Downloads/"$file"

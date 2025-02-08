#!/usr/bin/env bash

set -eufo pipefail

sha512Expected="ce92a0bb6914737390edd0851e4fed59feb8165645621aa059dbd3561d6f35f2303b2907332385f7e3a8b97a9d35dde2546044137591784e2fba157094e36cb9"

binaryName=tv
version=0.9.2
filename=tv-"$version"-linux-x86_64.tar.gz

curl -fsSL -O --output-dir ~/Downloads "https://github.com/alexpasmantier/television/releases/download/$version/tv-$version-linux-x86_64.tar.gz"

sha512Actual=$(sha512sum ~/Downloads/"$filename")

[ ! "${sha512Actual%% *}" = "$sha512Expected" ] &&  echo -e "$filename checksum check failed!\n${sha512Actual%% *} != $sha512Expected" && exit 1

tar -xzf ~/Downloads/"$filename" -C ~/Downloads

sudo install -o root -g root -m 0755  ~/Downloads/"$binaryName" /usr/local/bin/"$binaryName" && rm ~/Downloads/"$filename"

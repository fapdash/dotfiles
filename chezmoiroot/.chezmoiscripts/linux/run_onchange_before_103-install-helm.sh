#!/usr/bin/env bash

set -eufo pipefail

sha512Expected="ff4bf216fa81575779b1bfec0b778940051d70007ae04c4e38d87a952eed7157ec7288477a3f5139b2103570036ffa5bef596cca360e72439f0ad0217a3e7a9d"

binaryName=helm
version=3.16.4
filename=helm-v"$version"-linux-amd64.tar.gz

curl -fsSL -O --output-dir ~/Downloads "https://get.helm.sh/$filename"

sha512Actual=$(sha512sum ~/Downloads/"$filename")

[ ! "${sha512Actual%% *}" = "$sha512Expected" ] &&  echo -e "$filename checksum check failed!\n${sha512Actual%% *} != $sha512Expected" && exit 1

mkdir ~/Downloads/helm-extract
tar -xzf ~/Downloads/"$filename" -C ~/Downloads/helm-extract

sudo install -o root -g root -m 0755  ~/Downloads/helm-extract/linux-amd64/"$binaryName" /usr/local/bin/helm && rm ~/Downloads/"$filename"

rm -rf ~/Downloads/helm-extract

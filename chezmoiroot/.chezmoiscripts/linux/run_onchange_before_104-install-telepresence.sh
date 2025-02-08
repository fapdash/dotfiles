#!/usr/bin/env bash

set -eufo pipefail

sha512Expected="3a41d21f67a08af8723f3b2cbd389269fda392dbed63e66da98e95743951c061bbc84432a0fb042a2834ef207a166cc584833ad6844853e8ef2ba7e9cb5bf9c2"

binaryName=telepresence-linux-amd64

curl -fsSL -O --output-dir ~/Downloads "https://app.getambassador.io/download/tel2oss/releases/download/v2.21.1/$binaryName"

sha512Actual=$(sha512sum ~/Downloads/"$binaryName")

[ ! "${sha512Actual%% *}" = "$sha512Expected" ] &&  echo -e "$binaryName checksum check failed!\n${sha512Actual%% *} != $sha512Expected" && exit 1

sudo install -o root -g root -m 0755  ~/Downloads/"$binaryName" /usr/local/bin/telepresence && rm ~/Downloads/"$binaryName"

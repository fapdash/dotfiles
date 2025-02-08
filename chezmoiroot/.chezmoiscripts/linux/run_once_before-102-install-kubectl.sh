#!/usr/bin/env bash

set -eufo pipefail

sha512Expected="c2feb21002bceabd8779b91f9a87104ed168ca05ea87160dd94362504afb134e856adad3255120507c2d48d87a1e24e2cd95b294a06747598a46fd25f255bd9c"

binaryName=kubectl

curl -fsSL -O --output-dir ~/Downloads "https://dl.k8s.io/release/v1.32.0/bin/linux/amd64/$binaryName"

sha512Actual=$(sha512sum ~/Downloads/"$binaryName")

[ ! "${sha512Actual%% *}" = "$sha512Expected" ] &&  echo -e "$binaryName checksum check failed!\n${sha512Actual%% *} != $sha512Expected" && exit 1

sudo install -o root -g root -m 0755  ~/Downloads/"$binaryName" /usr/local/bin/kubectl && rm ~/Downloads/"$binaryName"

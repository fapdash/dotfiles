#!/usr/bin/env bash

set -eufo pipefail

sha512Expected="041aa328e39330edd1b2972fc04c0ca76b5c7c6cd929d65462e9ce0b7811353b5627155bf84fdf4046b2260b72e4f6cc13d398a3cfc175ceb0d70888d2527fb8"

curl -fsSL -O --output-dir ~/Downloads https://github.com/kubernetes/minikube/releases/latest/download/minikube-linux-amd64

sha512Actual=$(sha512sum ~/Downloads/minikube-linux-amd64)

[ ! "${sha512Actual%% *}" = "$sha512Expected" ] &&  echo -e "minikube checksum check failed!\n${sha512Actual%% *} != $sha512Expected" && exit 1

sudo install ~/Downloads/minikube-linux-amd64 /usr/local/bin/minikube && rm ~/Downloads/minikube-linux-amd64

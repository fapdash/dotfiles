#!/usr/bin/env bash

set -eufo pipefail

version=0.3.66
sha256Expected="a9158319adaaf1e93a1ecf100beb024b5c2b7920e14c13d7604f4d4ade95b250"

curl -fsSL -O --output-dir ~/.local/bin https://raw.githubusercontent.com/babashka/neil/v"$version"/neil

sha256Actual=$(sha256sum ~/.local/bin/neil)
[ ! "${sha256Actual%% *}" = "$sha256Expected" ] &&  echo -e "neil checksum check failed!\n${sha256Actual%% *} != $sha256Expected" && exit 1

chmod +x ~/.local/bin/neil

#!/usr/bin/env bash

set -eufo pipefail

pdfsamVersion="5.2.5"
pdfsamFile="pdfsam_$pdfsamVersion-1_amd64.deb"
sha256Expected="13d0727a01eb9dd3abb7bce70267dff8da23cf99a97df7c164f083f451a08062"

curl -fsSL -O --output-dir ~/Downloads "https://github.com/torakiki/pdfsam/releases/download/v$pdfsamVersion/$pdfsamFile"

sha256Actual=$(sha256sum ~/Downloads/$pdfsamFile)
[ ! "${sha256Actual%% *}" = "$sha256Expected" ] &&  echo -e "pdfsam checksum check failed!\n${sha256Actual%% *} != $sha256Expected" && exit 1

sudo apt-get install ~/Downloads/$pdfsamFile -y

rm ~/Downloads/$pdfsamFile

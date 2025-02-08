#!/usr/bin/env bash

set -eufo pipefail

sudo add-apt-repository -y ppa:obsproject/obs-studio

sudo apt install -y obs-studio

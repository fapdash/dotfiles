#!/usr/bin/env bash

set -eufo pipefail

sudo apt update && sudo apt install extrepo -y

sudo extrepo enable librewolf

sudo apt update && sudo apt install librewolf -y

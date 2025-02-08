#!/usr/bin/env bash

set -eufo pipefail

sudo apt install rustup -y

rustup component add rust-src
rustup component add rust-analyzer

cargo install cargo-watch
cargo install sqlx-cli

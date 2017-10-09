#! /usr/bin/env bash

set -eu -o pipefail

# Change into the directory that contains this script.
cd "$(dirname "$0")"

# Build the docker image.
docker build -t hex2017 .

# Enter the virtual environment and map this repository
# into the container at ~root/src.
exec docker run                       \
       --interactive                  \
       --tty                          \
       --rm=true                      \
       --name hex2017                 \
       --tmpfs /run                   \
       --tmpfs /tmp                   \
       -h hex2017                     \
       -v "$PWD/..":/root/src         \
       hex2017                        \
       /nix/var/nix/profiles/default/bin/bash -li

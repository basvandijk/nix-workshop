#! /bin/sh

# Change into the directory that contains this script.
cd "$(dirname "$0")" || exit 1

# Make sure we have the latest version of the container.
docker pull psimons/hex2017

# Enter the virtual environment and map this repository
# into the container at ~root/workshop.
exec docker run                 \
       --interactive            \
       --tty                    \
       -v "$PWD:/root/workshop" \
       psimons/hex2017          \
       "$@"

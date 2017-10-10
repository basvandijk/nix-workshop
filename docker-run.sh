#! /bin/sh

# Change into the directory that contains this script.
cd "$(dirname "$0")" || exit 1

# Enter the virtual environment and map this repository
# into the container at ~root/workshop.
exec docker run                 \
       --interactive            \
       --tty                    \
       -v "$PWD:/root/workshop" \
       psimons/hex2017          \
       "$@"

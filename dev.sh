#!/bin/sh
case "$1" in
    "") P=$(basename $PWD) ;;
    *) P="$1";;
esac

nix develop "git+ssh://git@github.com/seereason/sr-flake?dir=sr-libs&ref=main#$P" --refresh

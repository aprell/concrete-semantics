#!/usr/bin/env bash

set -eu

for exe in "$@"; do
    n="$(echo "$exe" | sed 's/[^0-9]*//g; s/^0//')"
    if dune exec "./$exe"; then
        printf "Chapter %2d: \e[32mOK\e[0m\n" "$n"
    else
        printf "Chapter %2d: \e[31mNG\e[0m\n" "$n"
    fi
done

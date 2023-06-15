#!/usr/bin/env bash

set -eu

for n in "$@"; do
    if dune exec "./chapter$(printf "%02d" "$n").exe"; then
        printf "Chapter %d: \e[32mOK\e[0m\n" "$n"
    else
        printf "Chapter %d: \e[31mNG\e[0m\n" "$n"
    fi
done

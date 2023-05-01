#!/usr/bin/env bash

set -eu

for n in "$@"; do
    n=$(printf "%02d" "$n")
    if dune exec "./chapter$n.exe"; then
        printf "Chapter %d: \e[32mOK\e[0m\n" "$n"
    else
        printf "Chapter %d: \e[31mNG\e[0m\n" "$n"
    fi
done

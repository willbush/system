#!/usr/bin/env bash

abort () {
    printf "Aborting! ERROR:\n$1\n"
    exit 1
}

if [[ ! -d /etc/nixos ]]; then
    msg="No /etc/nixos/ folder found.\n"
    msg+="This script is meant for NixOS and it typically has that folder."
    abort "$msg"
fi

if [[ $(whoami) != "root" ]]; then
    abort "You must run this script as root.\n"
fi

if [[ ! -e /etc/nixos/configuration.nix ]] ; then
  ln -sv $(pwd)/configuration.nix /etc/nixos/configuration.nix
fi

#!/usr/bin/env bash

# ensure this folder exists
mkdir -pv ~/.config/nixpkgs/

if [[ ! -e ~/.config/nixpkgs/home.nix ]] ; then
  ln -sv $(pwd)/home.nix ~/.config/nixpkgs/home.nix
fi

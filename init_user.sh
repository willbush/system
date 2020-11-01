#!/usr/bin/env bash

# ensure this folder exists
mkdir -pv ~/.config/nixpkgs/
ln -sv $(pwd)/home.nix ~/.config/nixpkgs/home.nix

#!/usr/bin/env bash

rsync -amvxx \
  --dry-run \
  --no-links \
  --exclude '/tmp/*' \
  --exclude '/root/*' \
  --exclude '.config/emacs/*' \
  ~/ /nix/persist/home/will/ \
  | rg -v '^skipping|/$'

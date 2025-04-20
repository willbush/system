#!/usr/bin/env bash

rsync -amvxx \
  --dry-run \
  --no-links \
  /nix/persist/ / \
  | rg -v '^skipping|/$'

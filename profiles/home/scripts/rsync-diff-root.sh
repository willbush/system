#!/usr/bin/env bash

rsync -amvxx \
  --dry-run \
  --no-links \
  --exclude '.config/emacs/*' \
  --exclude '/etc/.clean' \
  --exclude '/etc/group' \
  --exclude '/etc/NIXOS' \
  --exclude '/etc/passwd' \
  --exclude '/etc/printcap' \
  --exclude '/etc/shadow' \
  --exclude '/etc/subgid' \
  --exclude '/etc/subuid' \
  --exclude '/etc/sudoers' \
  --exclude '/etc/.updated' \
  --exclude '/root/*' \
  --exclude '/tmp/*' \
  --exclude '/var/cache/cups/*' \
  --exclude '/var/lib/cups/*' \
  --exclude '/var/lib/NetworkManager/*' \
  --exclude '/var/.updated' \
  / /nix/persist \
  | rg -v '^skipping|/$'

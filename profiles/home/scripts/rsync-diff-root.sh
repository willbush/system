#!/usr/bin/env bash

rsync -amvxx \
  --dry-run \
  --no-links \
  --exclude '.cache/claude-cli-nodejs/*' \
  --exclude '.cache/mpv/*' \
  --exclude '.config/emacs/*' \
  --exclude '.local/share/applications/claude-code-url-handler.desktop' \
  --exclude '.local/state/lesshst' \
  --exclude '/etc/.clean' \
  --exclude '/etc/group' \
  --exclude '/etc/logrotate.conf' \
  --exclude '/etc/NIXOS' \
  --exclude '/etc/passwd' \
  --exclude '/etc/printcap' \
  --exclude '/etc/shadow' \
  --exclude '/etc/subgid' \
  --exclude '/etc/subuid' \
  --exclude '/etc/sudoers' \
  --exclude '/etc/.updated' \
  --exclude '/etc/wpa_supplicant/imperative.conf' \
  --exclude '/root/*' \
  --exclude '/tmp/*' \
  --exclude '/var/cache/cups/*' \
  --exclude '/var/lib/cups/*' \
  --exclude '/var/lib/NetworkManager/*' \
  --exclude '/var/.updated' \
  / /nix/persist \
  | rg -v '^skipping|/$'

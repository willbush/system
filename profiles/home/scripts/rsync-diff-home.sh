#!/usr/bin/env bash

rsync -amvxx \
  --dry-run \
  --no-links \
  --exclude '/tmp/*' \
  --exclude '/root/*' \
  --exclude '.cache/claude-cli-nodejs/*' \
  --exclude '.cache/mpv/*' \
  --exclude '.config/emacs/*' \
  --exclude '.local/share/applications/claude-code-url-handler.desktop' \
  --exclude '.local/state/lesshst' \
  ~/ /nix/persist/home/will/ \
  | rg -v '^skipping|/$'

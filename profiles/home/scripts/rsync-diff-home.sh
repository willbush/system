#!/usr/bin/env bash

rsync -amvxx \
  --dry-run \
  --no-links \
  --exclude '.cache/claude-cli-nodejs/*' \
  --exclude '.cache/mpv/*' \
  --exclude '.config/emacs/*' \
  --exclude '.local/share/applications/claude-code-url-handler.desktop' \
  --exclude '.local/state/lesshst' \
  --exclude '/root/*' \
  --exclude '/tmp/*' \
  ~/ /nix/persist/home/will/ \
  | rg -v '^skipping|/$'

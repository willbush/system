#!/usr/bin/env bash

pane_id=$1
paths=$(yazi --chooser-file=/dev/stdout | while read -r; do printf "%q " "$REPLY"; done)

if [[ -n "$paths" ]]; then
  # \r for return
  echo -ne ":open $paths\r" | wezterm cli send-text --pane-id $pane_id --no-paste
fi

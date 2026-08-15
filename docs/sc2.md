# StarCraft II

Battle.net via Steam + Proton Experimental, running directly on the desktop.
Hyprland confines the cursor to the game window, which is what makes it
playable on a single ultrawide.

## Install

1. Steam → Add a Non-Steam Game → `Battle.net-Setup.exe`
2. Shortcut Properties → Compatibility → force **Proton Experimental**
3. Launch, install Battle.net + SC2. This creates the wine prefix at
   `~/.local/share/Steam/steamapps/compatdata/<appid>/pfx/`
4. Battle.net settings: exit launcher on game start

## Launch

Launch options are just:

```
mangohud %command%
```

- The shortcut Target stays `Battle.net-Setup.exe`. Changing it changes the
  appid and orphans the prefix. Running the setup again is harmless, it
  hands off to the installed client.
- `mangohud` caps frames at 144 (`programs.mangohud` in
  `profiles/home/programs.nix`). It has to be the launch prefix, since a
  bare `MANGOHUD=1` finds no layer in the Steam runtime. Shift_R+F12 shows
  the overlay, which is hidden by default.
- The `StarCraft II` windowrule in `configs/hypr/hyprland.lua` floats,
  centers, and confines the pointer to the window. Confinement is the whole
  point: SC2 issues a ClipCursor that wine's X11 driver drops, so the cursor
  escapes to the rest of the monitor without help.

## In-game settings

- Display mode **Windowed (Fullscreen)**. The game then sizes itself to
  2845x1600, which is 16:9 at monitor height, its widest supported aspect.
  It renders at that size natively, no scaling.
- Resolution and refresh pickers only appear in exclusive Fullscreen, which
  is unusable here since a monitor-sized window has no edges to confine
  against. "No valid resolutions found" in windowed modes is expected.
- SC2 refuses to be resized, so hyprland `size`/`min_size`/`max_size` rules
  on this window do nothing, even at map time. That also means a stray drag
  cannot resize it.
- vsync off, frames capped by mangohud. `cursorconfinemode=0` keeps the game
  out of confinement entirely, leaving it to hyprland.
- Settings live in `<pfx>/drive_c/users/steamuser/Documents/StarCraft II/Variables.txt`

## History

Two earlier approaches are in git history if this one ever regresses:

- **gamescope** (`git show 3153a801:docs/sc2.md`). Ran the game in a nested
  2560x1600 compositor at 144Hz, which gave real resolution and refresh
  control. Its cursor grab feeds the game raw unaccelerated deltas, and it
  discards the compositor's key repeat settings, so both needed working
  around.
- **wine virtual desktop** (`git show e3cbf1b7^:docs/sc2.md`). Confined the
  cursor by running the game inside `explorer.exe /desktop`. Chronic input
  death at launch, since wine engages its grab on a focus transition.

The prefix once held registry keys for those approaches (`Explorer\Desktops`,
`GrabFullscreen`, `UseTakeFocus`). They were removed on 2026-08-15 and the
cursor still confines without them. If registry edits are ever needed
again, use `reg.exe` while wine is running, and never edit `user.reg`
directly unless every wine process incl. `services.exe` is dead. From the
host: `steam-run python3 "<proton dir>/proton" run reg.exe ...` with
`STEAM_COMPAT_DATA_PATH=<compatdata/appid>` and
`STEAM_COMPAT_CLIENT_INSTALL_PATH=~/.local/share/Steam` exported.

# StarCraft II

Battle.net via Steam + Proton Experimental, run fullscreen inside gamescope.
Gamescope letterboxes the 2560x1600 game on the 3840x1600 monitor and
confines the cursor to the game area. Needs gamescope >= 3.16.25 (older
versions anchor the confinement region wrong for letterboxed games).

## Install

1. Steam → Add a Non-Steam Game → `Battle.net-Setup.exe`
2. Shortcut Properties → Compatibility → force **Proton Experimental**
3. Launch, install Battle.net + SC2. This creates the wine prefix at
   `~/.local/share/Steam/steamapps/compatdata/<appid>/pfx/`
4. Battle.net settings: exit launcher on game start

## Launch

Requires `programs.gamescope.enable` (`users/will.nix`). Launch options on
the shortcut:

```
gamescope -W 3840 -H 1600 -w 2560 -h 1600 -r 144 -o 30 --framerate-limit 144 -f --force-grab-cursor -- bash -c 'exec "${@:1:$#-1}" "C:\Program Files (x86)\Battle.net\Battle.net.exe"' -- %command%
```

- The bash slice swaps the trailing installer path for the installed
  Battle.net.exe. The shortcut Target stays `Battle.net-Setup.exe`, since
  changing it changes the appid and orphans the prefix.
- Game height equals output height, so the default fit scaler renders 1:1
  with pure side bars.
- `--force-grab-cursor` confines unconditionally instead of trusting the
  game's own confine request, which historically got dropped between wine
  and the display layer.
- `--framerate-limit 144` divides the `-r 144` refresh evenly, exact cap.
  `frameratecap=144` in `Variables.txt` stays as a backstop (lands ~166).

The `gamescope` windowrule in `configs/hypr/hyprland.lua` fullscreens the
window at map. Fullscreen focus also satisfies `vrr = 2`.

## In-game settings

- 2560x1600, windowed fullscreen, fills gamescope's nested display exactly
- vsync off, gamescope caps the frame rate
- Settings live in `<pfx>/drive_c/users/steamuser/Documents/StarCraft II/Variables.txt`

## Prefix leftovers

Inert registry keys from the earlier wine virtual-desktop approach remain in
the prefix: `Explorer\Desktops` `sc2`, `GrabFullscreen=Y` for `SC2_x64.exe`,
`UseTakeFocus=N`. Harmless under gamescope. If registry edits are ever
needed, use `reg.exe` while wine is running, and never edit `user.reg`
directly unless every wine process incl. `services.exe` is dead. From the
host: `steam-run python3 "<proton dir>/proton" run reg.exe ...` with
`STEAM_COMPAT_DATA_PATH=<compatdata/appid>` and
`STEAM_COMPAT_CLIENT_INSTALL_PATH=~/.local/share/Steam` exported.

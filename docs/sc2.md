# StarCraft II

Battle.net via Steam + Proton Experimental, run inside gamescope in a
2560x1600 borderless window on the 3840x1600 monitor. Hyprland confines the
cursor to that window, which keeps desktop mouse accel. Tested on gamescope
3.16.25.

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
gamescope -W 2560 -H 1600 -w 2560 -h 1600 -r 144 -o 30 -b -- bash -c '(for i in $(seq 15); do sleep 2; xset r rate 250 35; done) & exec "${@:1:$#-1}" "C:\Program Files (x86)\Battle.net\Battle.net.exe"' -- %command%
```

- The bash slice swaps the trailing installer path for the installed
  Battle.net.exe. The shortcut Target stays `Battle.net-Setup.exe`, since
  changing it changes the appid and orphans the prefix.
- Window size equals game size, so there is no letterboxing and no dead
  zones at the edges.
- No `--force-grab-cursor`. It confines, but gamescope then feeds the game
  raw unaccelerated deltas. Confinement comes from the `confine_pointer`
  windowrule in `configs/hypr/hyprland.lua` instead, so hyprland positions
  the cursor and its accel curve carries into the game.
- `xset r rate` matches the `input` repeat settings in
  `configs/hypr/hyprland.lua`. gamescope discards the compositor's
  repeat_info (`WaylandBackend.cpp` handler is empty) and hardcodes 25/600
  on its nested seat, so hyprland's values never reach the game. The bash
  runs as a gamescope child and inherits the nested `DISPLAY`, but xwayland
  applies gamescope's values only once the first game window takes keyboard
  focus, which is after the launch command has run. Hence the retry loop.
  It sticks once it wins, including across focus changes.
- The frame cap is `frameratecap=144` in `Variables.txt` (lands ~166,
  preferred over vsync for latency). gamescope's `--framerate-limit` is a
  refresh divisor that only affects vsynced apps, so it is not used.
- The window floats, so `vrr = 2` (fullscreen only) does not engage.

If confinement ever slips after tabbing away, the fallback is `-f
--force-grab-cursor` with `-W 3840`, trading mouse accel for a grab that
gamescope owns unconditionally.

## In-game settings

- 2560x1600, windowed fullscreen, fills gamescope's nested display exactly
- vsync off, capped by `frameratecap=144`. Turning vsync on instead gives
  an exact 144 against the 144Hz nested display, if ever wanted.
- Both the in-game sensitivity slider and gamescope `--mouse-sensitivity`
  are inert here. The game follows the absolute cursor, and gamescope only
  scales deltas while it holds the grab. Adjust speed in hyprland or via
  mouse DPI.
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

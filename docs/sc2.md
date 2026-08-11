# StarCraft II

Battle.net via Steam + Proton Experimental, with the cursor confined to the
game by running it inside a wine virtual desktop sized exactly to the game.

## Install

1. Steam → Add a Non-Steam Game → `Battle.net-Setup.exe`
2. Shortcut Properties → Compatibility → force **Proton Experimental**
3. Launch, install Battle.net + SC2. This creates the wine prefix at
   `~/.local/share/Steam/steamapps/compatdata/<appid>/pfx/`
4. Battle.net settings: exit launcher on game start

## Cursor confinement

Proton ignores wine's automatic virtual-desktop registry setting, so the
desktop is injected via launch options. Target stays untouched, since changing
it changes the appid and orphans the prefix:

```
mangohud bash -c 'exec "${@:1:$#-1}" explorer.exe /desktop=sc2,2560x1600 "C:\Program Files (x86)\Battle.net\Battle.net.exe"' -- %command%
```

Registry, in the prefix. Use `reg.exe` while wine is running, and never edit
`user.reg` directly unless every wine process incl. `services.exe` is dead:

```
reg add "HKCU\Software\Wine\Explorer\Desktops" /v sc2 /d 2560x1600 /f
reg add "HKCU\Software\Wine\AppDefaults\SC2_x64.exe\X11 Driver" /v GrabFullscreen /d Y /f
```

From the host: `steam-run python3 "<proton dir>/proton" run reg.exe ...` with
`STEAM_COMPAT_DATA_PATH=<compatdata/appid>` and
`STEAM_COMPAT_CLIENT_INSTALL_PATH=~/.local/share/Steam` exported.

The `sc2 - Wine Desktop` windowrule in `configs/hypr/hyprland.lua` floats it
centered at 2560x1600, borderless, and blacks out everything around it.

## Frame cap and monitoring

The virtual desktop reports a fake 60Hz monitor, so the in-game refresh
setting only offers 60 and vsync paces to 60. `DXVK_FRAME_RATE` was removed
in DXVK 3.0. Uncapped runs 500+ fps and pegs the GPU. What works:

- MangoHud `fps_limit=144`, exact. Configured via `programs.mangohud` in
  home-manager, applied by the `mangohud` prefix in the launch options.
  Also the monitoring overlay: fps, frametimes, temps, throttling. Toggle
  with Shift_R+F12. All keybinds are pinned to right shift because the
  Shift_L+F1..F4 defaults collide with shift-queueing.
- `frameratecap=144` in `Variables.txt` as backstop, overshoots to ~166.
- `dxvk.maxFrameRate` in a conf file via `DXVK_CONFIG_FILE` also works.

## In-game settings

- 2560x1600, windowed fullscreen, fills the desktop window exactly
- vsync off, fps capped by MangoHud
- Settings live in `<pfx>/drive_c/users/steamuser/Documents/StarCraft II/Variables.txt`

The cursor grab engages when the game gains focus. If it isn't confining,
switch workspaces away and back.

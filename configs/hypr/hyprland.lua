hl.monitor({ output = "DP-1", mode = "highrr", position = "auto", scale = 1 })

-- CTRL key
local c = "CTRL"
-- SUPER key
local s = "SUPER"
-- SUPER + SHIFT key
local ss = "SUPER + SHIFT"
-- SUPER + CTRL key
local sc = "SUPER + CTRL"

-- programs
local terminal = "ghostty --working-directory=home"
local menu = "tofi-drun --drun-launch=true"
local lazygit = 'lazygit --path "$(hyprcwd)"'
local firefox = "firefox"
local spotify = "spotify"

hl.bind(c .. " + Space", hl.dsp.exec_cmd(menu))
hl.bind(s .. " + L", hl.dsp.exec_cmd("hyprlock"))

hl.bind(ss .. " + C", hl.dsp.window.close()) -- [C]lose
hl.bind(ss .. " + BackSpace", hl.dsp.exec_cmd(firefox))
hl.bind(ss .. " + Apostrophe", hl.dsp.exec_cmd(terminal .. " -e " .. lazygit))
hl.bind(ss .. " + Return", hl.dsp.exec_cmd(terminal))

hl.bind(ss .. " + Y", hl.dsp.exec_cmd("grim")) -- take a screenshot
hl.bind(ss .. " + X", hl.dsp.exit()) -- e[X]it

-- xmonad like promote to master
hl.bind(s .. " + Return", hl.dsp.layout("swapwithmaster master"))
hl.bind(s .. " + Space", hl.dsp.layout("orientationcycle"))

-- Reset back to my default master layout orientation
hl.bind(ss .. " + Space", function()
  hl.dispatch(hl.dsp.layout("orientationright"))
  hl.dispatch(hl.dsp.layout("mfact exact 0.75"))
end)

-- Cycle focus to the next / prev window.
hl.bind(s .. " + N", hl.dsp.layout("cyclenext"))
hl.bind(s .. " + E", hl.dsp.layout("cycleprev"))

-- Swap focused window to the next / prev window.
hl.bind(ss .. " + N", hl.dsp.layout("swapnext"))
hl.bind(ss .. " + E", hl.dsp.layout("swapprev"))

-- Rotate the next / prev window in stack to be the master, while keeping the
-- focus on master
hl.bind(sc .. " + N", hl.dsp.layout("rollnext"))
hl.bind(sc .. " + E", hl.dsp.layout("rollprev"))

-- Expand / Shrink master window.
hl.bind(s .. " + M", hl.dsp.layout("mfact +0.05"))
hl.bind(s .. " + I", hl.dsp.layout("mfact -0.05"))

-- ma[X]imize: toggle fullscreen (keep gaps and bars)
hl.bind(s .. " + X", hl.dsp.window.fullscreen({ mode = "maximized" }))
-- [D]rop: Toggle floating on current window.
hl.bind(s .. " + D", hl.dsp.window.float())
-- [C]enter: recenter floating window
hl.bind(s .. " + C", hl.dsp.window.center())

-- These are Colemak-DH keys. I don't dedicated number row unless I go
-- through a layer key. So I rather use keys close to the home row.

-- binds s + {Q..G} to workspace {Q..G}
-- binds s + shift + {Q..G} to move to workspace {Q..G}
local workspace_keys = { "Q", "W", "F", "P", "B", "A", "R", "S", "T", "G" }
for i, key in ipairs(workspace_keys) do
  hl.bind(s .. " + " .. key, hl.dsp.focus({ workspace = i }))
  hl.bind(ss .. " + " .. key, hl.dsp.window.move({ workspace = i, follow = false }))
end

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(s .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(s .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Use `hyprctl clients` to see window properties
hl.window_rule({ match = { class = "^(com\\.mitchellh\\.ghostty)$" }, opacity = "0.98 0.88" })
hl.window_rule({ match = { class = "^(emacs)$" }, opacity = "0.98 0.88" })

hl.window_rule({ match = { initial_title = "^viewer.*$" }, float = true, fullscreen = true })
hl.window_rule({ match = { initial_title = "^Metric dashboards$" }, float = true, persistent_size = true })

-- SC2 in a game-sized wine virtual desktop so wine can confine the cursor
hl.window_rule({
  match = { title = "^(sc2 - Wine Desktop)$" },
  float = true,
  center = true,
  size = "2560 1600",
  min_size = { 2560, 1600 },
  max_size = { 2560, 1600 },
  border_size = 0,
  rounding = 0,
  dim_around = true,
  no_shadow = true,
})

-- Smart gaps:
-- see https://wiki.hyprland.org/Configuring/Workspace-Rules/#smart-gaps
hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]", gaps_out = 0, gaps_in = 0 })
-- Remove borders/rounding for tiled windows on those workspaces
hl.window_rule({ match = { workspace = "w[tv1]", float = false }, border_size = 0, rounding = 0 })
hl.window_rule({ match = { workspace = "f[1]", float = false }, border_size = 0, rounding = 0 })

hl.config({
  general = {
    layout = "master",
  },

  decoration = {
    -- dim_around rule dims to full black
    dim_around = 1.0,
  },

  input = {
    repeat_delay = 250, -- default 600ms
    repeat_rate = 35, -- default 25/s
  },

  master = {
    -- new window is master
    new_status = "master",
    -- new window pushed to the top of the stack.
    new_on_top = true,
    -- The split ratio between 0.0 and 1.0
    mfact = 0.75,
    -- The default placement of the master area, can be left, right, top, bottom or
    -- center. I like right be cause I tend to use an ultra-wide screen and that
    -- puts the master window close to optimal viewing.
    orientation = "right",
  },

  -- See https://wiki.hyprland.org/Configuring/Variables/ for more
  misc = {
    -- Set to 0 or 1 to disable the anime mascot wallpapers
    force_default_wallpaper = 0,
    -- TODO remove once viewer fixed
    enable_anr_dialog = false,
    -- Adaptive sync, but only while a fullscreen app is focused
    vrr = 2,
  },
})

hl.animation({ leaf = "workspaces", enabled = true, speed = 3, bezier = "default" })

-- Startup these programs on launch.
hl.on("hyprland.start", function()
  hl.exec_cmd(terminal, { workspace = "1 silent" })
  hl.exec_cmd(terminal, { workspace = "1 silent" })
  hl.exec_cmd(firefox, { workspace = "2 silent" })
  hl.exec_cmd(spotify, { workspace = "3 silent" })
end)

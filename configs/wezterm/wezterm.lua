local wezterm = require("wezterm")
local act = wezterm.action
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
local io = require("io")
local os = require("os")

-- Show which key table is active in the status area
wezterm.on("update-right-status", function(window, _)
	local name = window:active_key_table()
	if name then
		name = "MODE: " .. name
	end
	window:set_right_status(name or "")
end)

-- Eliminates need for a copy mode. Think I'll use this over search mode too.
wezterm.on("trigger-hx-with-scrollback", function(window, pane)
	local text = pane:get_lines_as_text(pane:get_dimensions().scrollback_rows)

	local name = os.tmpname()
	local f = io.open(name, "w+")

	if not f then
		return
	end

	f:write(text)
	f:flush()
	f:close()

	window:perform_action(
		act.SpawnCommandInNewWindow({
			args = { "hx", name },
		}),
		pane
	)
end)

return {
	check_for_updates = false,
	default_workspace = "~",
	disable_default_key_bindings = true,
	hide_tab_bar_if_only_one_tab = false,
	tab_and_split_indices_are_zero_based = true,
	tab_bar_at_bottom = true,
	use_fancy_tab_bar = false,
	window_padding = {
		left = "1cell",
		right = "1cell",
		top = "0.5cell",
		bottom = "0cell",
	},

	-- Define the leader key
	leader = { key = "g", mods = "CTRL", timeout_milliseconds = 1000 },
	colors = {
		-- change the cursor color when IME, dead key, or leader key composition states
		-- are active.
		compose_cursor = "orange",
	},

	keys = {
		-- Non LEADER keys:
		{ key = "0", mods = "CTRL", action = act.ResetFontSize },
		{ key = "-", mods = "CTRL", action = act.DecreaseFontSize },
		{ key = "+", mods = "SHIFT|CTRL", action = act.IncreaseFontSize }, -- actually C-+ due to QMK quirk

		-- copy / paste
		{ key = "C", mods = "CTRL", action = act.CopyTo("Clipboard") },
		{ key = "V", mods = "CTRL", action = act.PasteFrom("Clipboard") },

		-- tab control
		{ key = "Tab", mods = "CTRL", action = act.ActivateTabRelative(1) },
		{ key = "Tab", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(-1) },
		{ key = "T", mods = "CTRL", action = act.SpawnTab("CurrentPaneDomain") },
		{ key = "W", mods = "CTRL", action = act.CloseCurrentTab({ confirm = true }) },

		-- LEADER based keys:
		{
			key = "phys:Space",
			mods = "LEADER",
			action = act.ActivateCommandPalette,
		},
		{
			key = "Enter",
			mods = "LEADER",
			-- similar to jump to word in hx
			action = act.QuickSelect,
		},
		{
			key = "U",
			mods = "LEADER",
			-- Select and copy emoji
			action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
		},
		{
			key = "L",
			mods = "LEADER",
			action = act.ClearScrollback("ScrollbackOnly"),
		},
		{
			key = "D",
			mods = "LEADER",
			action = act.ShowDebugOverlay,
		},

		-- workspace_switcher
		{
			key = "w",
			mods = "LEADER",
			action = workspace_switcher.switch_workspace(),
		},
		{
			key = "phys:Tab",
			mods = "LEADER",
			action = workspace_switcher.switch_to_prev_workspace(),
		},
		-- Spawn commands
		{
			key = "g",
			mods = "LEADER",
			action = act.SpawnCommandInNewWindow({
				args = { "lazygit" },
			}),
		},
		{
			key = "E",
			mods = "LEADER",
			action = act.EmitEvent("trigger-hx-with-scrollback"),
		},

		-- Vertical split with leader+v
		-- NOTE wezterm named these backwards from vim..
		{
			key = "v",
			mods = "LEADER",
			action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }),
		},
		-- Horizontal split with leader+s
		{
			key = "s",
			mods = "LEADER",
			action = act.SplitVertical({ domain = "CurrentPaneDomain" }),
		},
		-- Panes
		{
			mods = "LEADER",
			key = "n",
			action = act.ActivatePaneDirection("Prev"),
		},
		{
			mods = "LEADER",
			key = "e",
			action = act.ActivatePaneDirection("Next"),
		},
		{
			key = "x",
			mods = "LEADER",
			action = act.TogglePaneZoomState,
		},
		-- Custom sticky modes
		{
			key = "p",
			mods = "LEADER",
			action = act.ActivateKeyTable({
				name = "pane_mode",
				one_shot = false,
			}),
		},
		{
			key = "z",
			mods = "LEADER",
			action = act.ActivateKeyTable({
				name = "view_mode",
				one_shot = false,
			}),
		},
	},

	key_tables = {
		pane_mode = {
			-- escape hatch
			{ key = "Escape", action = "PopKeyTable" },

			{ key = "n", action = act.ActivatePaneDirection("Prev") },
			{ key = "e", action = act.ActivatePaneDirection("Next") },

			{ key = "M", action = act.AdjustPaneSize({ "Left", 5 }) },
			{ key = "N", action = act.RotatePanes("CounterClockwise") },
			{ key = "E", action = act.RotatePanes("Clockwise") },
			{ key = "I", action = act.AdjustPaneSize({ "Right", 5 }) },

			{ key = "x", action = act.TogglePaneZoomState },
		},
		view_mode = {
			-- escape hatch
			{ key = "Escape", action = "PopKeyTable" },

			{ key = "e", action = act.ScrollByLine(-1) },
			{ key = "n", action = act.ScrollByLine(1) },

			{ key = "E", action = act.ScrollByPage(-1) },
			{ key = "N", action = act.ScrollByPage(1) },
		},
	},
}

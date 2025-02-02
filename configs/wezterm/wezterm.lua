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

-- Eliminates need for a copy mode.
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
		{ key = "Tab", mods = "CTRL", action = act.ActivateTabRelative(1) },
		{ key = "Tab", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(-1) },

		-- workspace_switcher
		{
			key = "w",
			mods = "LEADER",
			action = workspace_switcher.switch_workspace(),
		},
		{
			key = "W",
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
		{
			mods = "LEADER",
			key = "m",
			action = act.AdjustPaneSize({ "Left", 5 }),
		},
		-- Panes
		{
			mods = "LEADER",
			key = "n",
			action = act.RotatePanes("CounterClockwise"),
		},
		{
			mods = "LEADER",
			key = "e",
			action = act.RotatePanes("Clockwise"),
		},
		{
			mods = "LEADER",
			key = "i",
			action = act.AdjustPaneSize({ "Right", 5 }),
		},
		{
			key = "p",
			mods = "LEADER",
			action = act.ActivateKeyTable({
				name = "pane_mode",
				one_shot = false,
			}),
		},

		{ key = "0", mods = "CTRL", action = act.ResetFontSize },
		{ key = "-", mods = "CTRL", action = act.DecreaseFontSize },
		{ key = "+", mods = "SHIFT|CTRL", action = act.IncreaseFontSize }, -- actually C-+ due to QMK quirk

		{ key = "F", mods = "CTRL", action = act.Search("CurrentSelectionOrEmptyString") },
		{ key = "K", mods = "CTRL", action = act.ClearScrollback("ScrollbackOnly") },
		{ key = "L", mods = "CTRL", action = act.ShowDebugOverlay },
		{ key = "M", mods = "CTRL", action = act.Hide },
		{ key = "N", mods = "CTRL", action = act.SpawnWindow },
		{ key = "P", mods = "CTRL", action = act.ActivateCommandPalette },
		{ key = "R", mods = "CTRL", action = act.ReloadConfiguration },
		{ key = "T", mods = "CTRL", action = act.SpawnTab("CurrentPaneDomain") },
		{ key = "Z", mods = "CTRL", action = act.TogglePaneZoomState },

		{
			key = "U",
			mods = "CTRL",
			action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
		},

		{ key = "W", mods = "CTRL", action = act.CloseCurrentTab({ confirm = true }) },

		{ key = "C", mods = "CTRL", action = act.CopyTo("Clipboard") },

		{ key = "V", mods = "CTRL", action = act.PasteFrom("Clipboard") },

		{ key = "w", mods = "SHIFT|CTRL", action = act.CloseCurrentTab({ confirm = true }) },
		{ key = "phys:Space", mods = "SHIFT|CTRL", action = act.QuickSelect },

		{ key = "PageUp", mods = "SHIFT", action = act.ScrollByPage(-1) },
		{ key = "PageDown", mods = "SHIFT", action = act.ScrollByPage(1) },
	},

	key_tables = {
		pane_mode = {
			-- escape hatch
			{ key = "Escape", action = "PopKeyTable" },

			{ key = "m", action = act.AdjustPaneSize({ "Left", 5 }) },
			{ key = "n", action = act.RotatePanes("CounterClockwise") },
			{ key = "e", action = act.RotatePanes("Clockwise") },
			{ key = "i", action = act.AdjustPaneSize({ "Right", 5 }) },
		},

		search_mode = {
			{ key = "Enter", action = act.CopyMode("PriorMatch") },
			{ key = "Escape", action = act.CopyMode("Close") },
			{ key = "n", mods = "CTRL", action = act.CopyMode("NextMatch") },
			{ key = "p", mods = "CTRL", action = act.CopyMode("PriorMatch") },
			{ key = "r", mods = "CTRL", action = act.CopyMode("CycleMatchType") },
			{ key = "u", mods = "CTRL", action = act.CopyMode("ClearPattern") },
			{ key = "PageUp", action = act.CopyMode("PriorMatchPage") },
			{ key = "PageDown", action = act.CopyMode("NextMatchPage") },
			{ key = "UpArrow", action = act.CopyMode("PriorMatch") },
			{ key = "DownArrow", action = act.CopyMode("NextMatch") },
		},
	},
}

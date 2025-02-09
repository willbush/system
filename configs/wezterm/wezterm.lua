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

-- An helper function to perform an action then pop key table.
local function with_pop(action)
	return act.Multiple({ action, "PopKeyTable" })
end

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
			key = "U",
			mods = "LEADER",
			-- Select and copy emoji
			action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
		},
		{
			key = "D",
			mods = "LEADER",
			action = act.ShowDebugOverlay,
		},

		-- workspace_switcher (mnemonic p for project)
		{
			key = "p",
			mods = "LEADER",
			action = workspace_switcher.switch_workspace(),
		},
		{
			key = "p",
			mods = "LEADER|CTRL", -- allows C-a C-p
			action = workspace_switcher.switch_workspace(),
		},
		{
			key = "phys:Tab",
			mods = "LEADER",
			action = workspace_switcher.switch_to_prev_workspace(),
		},
		{
			key = "phys:Tab",
			mods = "LEADER|CTRL",
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
			key = "g",
			mods = "LEADER|CTRL",
			action = act.SpawnCommandInNewWindow({
				args = { "lazygit" },
			}),
		},
		{
			key = "E",
			mods = "LEADER",
			action = act.EmitEvent("trigger-hx-with-scrollback"),
		},

		-- Custom sticky modes
		-- mnemonic w for window (similar to helix)
		{
			key = "w",
			mods = "LEADER",
			action = act.ActivateKeyTable({
				name = "window_mode",
				one_shot = false,
				timeout_milliseconds = 5000,
			}),
		},
		{
			key = "w",
			mods = "LEADER|CTRL",
			action = act.ActivateKeyTable({
				name = "window_mode",
				one_shot = false,
				timeout_milliseconds = 5000,
			}),
		},
		{
			key = "z",
			mods = "LEADER",
			action = act.ActivateKeyTable({
				name = "view_mode",
				one_shot = false,
				timeout_milliseconds = 5000,
			}),
		},
		{
			key = "z",
			mods = "LEADER|CTRL",
			action = act.ActivateKeyTable({
				name = "view_mode",
				one_shot = false,
				timeout_milliseconds = 5000,
			}),
		},
	},

	key_tables = {
		-- Windows in wezterm are gimped compared to something like hyprland. Never want
		-- to have more than one split because there's no way to balance them. However,
		-- this simplifies bindings because I'm only ever dealing with one split.
		window_mode = {
			-- escape hatch
			{ key = "Escape", action = "PopKeyTable" },

			-- Vertical split
			-- NOTE wezterm named these backwards from vim..
			{ key = "v", action = with_pop(act.SplitHorizontal({ domain = "CurrentPaneDomain" })) },
			{ key = "v", mods = "CTRL", action = with_pop(act.SplitHorizontal({ domain = "CurrentPaneDomain" })) },
			-- Horizontal split
			{ key = "s", action = with_pop(act.SplitVertical({ domain = "CurrentPaneDomain" })) },
			{ key = "s", mods = "CTRL", action = with_pop(act.SplitVertical({ domain = "CurrentPaneDomain" })) },
			-- movement
			{ key = "w", action = with_pop(act.ActivatePaneDirection("Prev")) },
			{ key = "w", mods = "CTRL", action = with_pop(act.ActivatePaneDirection("Prev")) },
			-- pane adjustment
			-- rotate similar to vim
			{ key = "r", action = with_pop(act.RotatePanes("CounterClockwise")) },
			{ key = "r", mods = "CTRL", action = with_pop(act.RotatePanes("CounterClockwise")) },
			-- kill
			{ key = "k", action = with_pop(act.CloseCurrentPane({ confirm = true })) },
			{ key = "k", mods = "CTRL", action = with_pop(act.CloseCurrentPane({ confirm = true })) },
			-- zoom
			{ key = "x", action = with_pop(act.TogglePaneZoomState) },
			{ key = "x", mods = "CTRL", action = with_pop(act.TogglePaneZoomState) },
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

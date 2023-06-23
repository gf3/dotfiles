local wezterm = require 'wezterm'
local act = wezterm.action

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Ayu Mirage'
  else
    -- return 'rose-pine-dawn'
    return 'Yousai (terminal.sexy)'
    -- return 'Horizon Light (base16)'
  end
end

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = utf8.char(0xe0b0)

return {
  audible_bell = "Disabled",
  font = wezterm.font 'JetBrainsMono Nerd Font',
  font_size = 15.0,
  color_scheme = scheme_for_appearance(get_appearance()),
  tab_bar_at_bottom = true,
  visual_bell = {
    fade_in_function = 'EaseIn',
    fade_in_duration_ms = 75,
    fade_out_function = 'EaseOut',
    fade_out_duration_ms = 75,
  },
  colors = {
    visual_bell = '#202020',
  },
  window_decorations = "RESIZE",
  keys = {
    {
      key = 'k',
      mods = 'CMD',
      action = act.Multiple {
        -- act.SendKey { key = 'l', mods = 'CTRL' },
        act.ClearScrollback 'ScrollbackAndViewport',
      },
    },
    {
      key = 'k',
      mods = 'CTRL|SHIFT',
      action = act.Multiple {
        -- act.SendKey { key = 'l', mods = 'CTRL' },
        act.ClearScrollback 'ScrollbackAndViewport',
      },
    },
  },
}

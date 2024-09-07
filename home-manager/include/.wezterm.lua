local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.color_scheme = 'Solarized Dark Higher Contrast'

config.font = wezterm.font_with_fallback {
    {family='Operator Mono Lig', weight="Book"},
    {family='Operator Mono Lig', weight="DemiLight"},
    'Operator Mono',
    'JetBrains Mono',
    'Fira Code'
}
config.window_background_opacity = 0.85

-- right Alt key should just be Alt
config.send_composed_key_when_right_alt_is_pressed = false

config.swap_backspace_and_delete = false

return config

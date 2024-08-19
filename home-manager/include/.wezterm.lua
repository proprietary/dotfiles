local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.color_scheme = 'ChallengerDeep'

config.window_frame = {
    font = wezterm.font { family = 'Operator Mono' },
    font_size = 12.0,
}
config.window_background_opacity = 0.90

return config

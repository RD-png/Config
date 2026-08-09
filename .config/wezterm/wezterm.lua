local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

config.enable_tab_bar = false
config.term = 'xterm-256color'
config.colors = require 'modus'
-- config.bold_brightens_ansi_colors = "No"

config.font = wezterm.font('Fantasque Sans Mono')
config.font_size = 18

config.scrollback_lines = 10000
config.window_background_opacity = 1.0
config.window_padding = {
  left = 20,
  right = 5,
  top = 20,
  bottom = 20,
}

config.keys = {
   { key = 'P',
     mods = 'CTRL|SHIFT',
     action = act.ScrollByLine(-1),
   },

   { key = 'N',
     mods = 'CTRL|SHIFT',
     action = act.ScrollByLine(1),
   },

   { key = 'v',
     mods = 'CTRL',
     action = act.ScrollByPage(1),
   },

   { key = 'v',
     mods = 'ALT',
     action = act.ScrollByPage(-1),
   },

   { key = "y",
     mods = "ALT",
     action = wezterm.action.PasteFrom "Clipboard",
   },

   { key = 'V',
     mods = 'CTRL|SHIFT',
     action = act.ScrollToBottom,
   },

   { key = 'V',
     mods = 'ALT|SHIFT',
     action = act.ScrollToTop,
   },

   { key = '+',
     mods = 'CTRL',
     action = act.IncreaseFontSize,
   },

   { key = '-',
     mods = 'CTRL',
     action = act.DecreaseFontSize,
   },

   { key = 's',
     mods = 'CTRL',
     action = act.Search 'CurrentSelectionOrEmptyString',
   },
}

config.key_tables = {
   search_mode = {
      { key = 'Enter',
        action = act.Multiple {
           act.CopyMode 'AcceptPattern',
           act.CopyMode { SetSelectionMode = 'Cell' }
        },
      },

      { key = 'g',
        mods = 'CTRL',
        action = act.CopyMode 'Close',
      },

      { key = 'c',
        mods = 'CTRL',
        action = act.CopyMode 'Close',
      },

      { key = 'u',
        mods = 'CTRL',
        action = act.CopyMode 'ClearPattern',
      },


      { key = 'Backspace',
        mods = 'CTRL',
        action = act.CopyMode 'EditPattern',
      },

      { key = 'p',
        mods = 'ALT',
        action = act.CopyMode 'PriorMatch',
      },

      { key = 'n',
        mods = 'ALT',
        action = act.CopyMode 'NextMatch',
      },

      { key = 'UpArrow',
        action = act.CopyMode 'PriorMatch',
      },

      { key = 'DownArrow',
        action = act.CopyMode 'NextMatch',
      },

      { key = 'p',
        mods = 'CTRL',
        action = act.CopyMode 'PriorMatch',
      },

      { key = 'n',
        mods = 'CTRL',
        action = act.CopyMode 'NextMatch',
      },
   },
   copy_mode = {
      { key = 'g',
        mods = 'CTRL',
        action = act.CopyMode 'EditPattern'
      },

      { key = 'p',
        mods = 'CTRL',
        action = act.CopyMode 'MoveUp'
      },

      { key = 'n',
        mods = 'CTRL',
        action = act.CopyMode 'MoveDown'
      },

      { key = 'f',
        mods = 'CTRL',
        action = act.CopyMode 'MoveRight'
      },

      { key = 'b',
        mods = 'CTRL',
        action = act.CopyMode 'MoveLeft'
      },

      { key = 'a',
        mods = 'CTRL',
        action = act.CopyMode 'MoveToStartOfLineContent'
      },

      { key = 'e',
        mods = 'CTRL',
        action = act.CopyMode 'MoveToEndOfLineContent'
      },

      { key = ' ',
        mods = 'CTRL',
        action = act.CopyMode { SetSelectionMode = 'Cell' },
      },

      { key = 'w',
        mods = 'ALT',
        action = act.Multiple {
           act.CopyTo "ClipboardAndPrimarySelection",
           act.CopyMode { SetSelectionMode = 'Cell' }
        },
      },
   },
}

return config

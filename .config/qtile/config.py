# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os
import subprocess
import socket
import psutil
import re
from typing import List

from libqtile import qtile, hook
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy

mod = "mod4"
terminal = "alacritty"


keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Window Movement
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.group.next_window()),
    Key([mod], "k", lazy.group.prev_window()),
    Key([mod], "o", lazy.next_screen()),
    # Window Control
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "b", lazy.hide_show_bar(), desc="Hides the bar"),
    Key([mod, "shift"], "f", lazy.window.toggle_floating()),
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "q", lazy.window.kill()),
    # Window Arrangement
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod], "t", lazy.layout.normalize()),
    Key([mod, "control"], "f", lazy.window.toggle_floating(), desc='Toggle floating'),
    # Spawn Programs
    # Key([mod], "d", lazy.spawn("dmenu_run -fn 'Misc Termsyn.Icons:size=15.0'")),
    Key([mod], "d", lazy.spawn("rofi -show drun - dpi 0")),
    Key([mod], "s", lazy.spawn("rofi -show window - dpi 0")),
    Key([mod], "d", lazy.spawn("rofi -show drun -dpi 0")),
    Key([mod], "s", lazy.spawn("rofi -show window -dpi 0")),
    Key([mod], "w", lazy.spawn("brave")),
    Key([mod, "shift"], "d", lazy.spawn("vesktop")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pamixer --allow-boost -i 10")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pamixer -d 10")),
    Key([], "XF86AudioMute", lazy.spawn("pamixer -t")),
    Key([], "XF86AudioMicMute", lazy.spawn("pamixer --source 47 -t")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("light -A 10")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("light -U 10")),
    Key([], "XF86AudioPlay", lazy.spawn("playerctl play-pause")),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next")),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous")),
    Key([mod, "shift"], "p", lazy.spawn("postman")),
    Key([mod, "shift"], "e", lazy.spawn("emacs")),
    Key([mod, "shift"], "s", lazy.spawn("flameshot gui")),
    Key([mod, "shift"], "Return", lazy.spawn("pcmanfm")),
    Key([mod], "Return", lazy.spawn(terminal)),
    # Qtile Config Commands
    Key([mod, "shift"], "r", lazy.reload_config()),
    Key([mod, "shift"], "q", lazy.shutdown()),
    # Misc
    Key(
        [mod, "shift"],
        "t",
        lazy.spawn(os.path.expanduser("~/.config/Dmenu/scripts/alac-theme")),
    ),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(toggle=True),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

    layout_theme = {
        "border_width": 5,
        "margin": 0,
        "border_focus": "#cd5c5c",
        "border_normal": "#444444",
    }

layouts = [
    layout.Columns(**layout_theme, border_on_single=False, insert_position=1),
    layout.Max(**layout_theme),
    layout.Floating(**layout_theme),
]

# Append scratchpad with dropdowns to groups
groups.append(
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "term", "alacritty", width=0.5, height=0.5, x=0.275, y=0.2, opacity=1
            ),
        ],
    )
)
# extend keys list with keybinding for scratchpad
keys.extend(
    [
        Key(
            [mod, "control"], "Return", lazy.group["scratchpad"].dropdown_toggle("term")
        ),
    ]
)

widget_defaults = dict(
    fontsize=25,
    font="fantasquesansmono",
    padding=2,
)
extension_defaults = widget_defaults.copy()

colors = [
    ["#D9E0EE", "#D9E0EE"],  # foreground
    ["#161320", "#161320"],  # background
    ["#3b4252", "#3b4252"],  # background lighter
    ["#F28FAD", "#F28FAD"],  # red
    ["#ABE9B3", "#ABE9B3"],  # green
    ["#FAE3B0", "#FAE3B0"],  # yellow
    ["#96CDFB", "#96CDFB"],  # blue
    ["#DDB6F2", "#DDB6F2"],  # magenta
    ["#89DCEB", "#89DCEB"],  # cyan
    ["#C3BAC6", "#C3BAC6"],  # white
    ["#6E6C7E", "#6E6C7E"],  # grey
    ["#F8BD96", "#F8BD96"],  # orange
    ["#96CDFB", "#96CDFB"],  # super cyan
    ["#5e81ac", "#5e81ac"],  # super blue
    ["#242831", "#242831"],  # super dark background
]

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    active=colors[0],
                    highlight_method="line",
                    highlight_color="#cd5c5c",
                    padding_x=6,
                    borderwidth=0,
                    margin_x=0,
                    disable_drag=True,
                    block_highlight_text_color="FFFFFF",
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.CurrentLayoutIcon(
                    foreground=colors[0]
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.Prompt(),
                widget.WindowName(
                    foreground=colors[0]
                ),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Systray(
                    icon_size=22,
                    padding=20
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    foreground=colors[7],
                    text="",
                    font="Font Awesome 5 Free Solid",
                ),
                widget.CPU(
                    foreground=colors[0],
                    update_interval=1,
                    format="{load_percent: 3.0f}%",
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    text="",
                    font="Font Awesome 5 Free Solid",
                    foreground=colors[6],
                ),
                widget.Memory(
                    foreground=colors[0],
                    format="{MemPercent: 3.0f}%",
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    foreground=colors[3],
                    text=" ",
                    font="Font Awesome 5 Free Solid",
                ),
                widget.Clock(
                    foreground=colors[0],
                    format="%a %d-%m-%y"
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    foreground=colors[11],
                    text=" ",
                    font="Font Awesome 5 Free Solid",
                ),
                widget.Clock(
                    foreground=colors[0],
                    format="%H:%M"
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
            ],
            size=27,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    highlight_method="line",
                    highlight_color="#cd5c5c",
                    padding_x=6,
                    borderwidth=0,
                    margin_x=0,
                    disable_drag=True,
                    block_highlight_text_color="FFFFFF",
                ),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.TextBox(
                    foreground=colors[7],
                    text="",
                    font="Font Awesome 5 Free Solid",
                ),
                widget.CPU(
                    foreground=colors[0],
                    update_interval=1,
                    format="{load_percent: 3.0f}%",
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    text="",
                    font="Font Awesome 5 Free Solid",
                    foreground=colors[6],
                ),
                widget.Memory(
                    foreground=colors[0],
                    format="{MemPercent: 3.0f}%",
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    foreground=colors[3],
                    text=" ",
                    font="Font Awesome 5 Free Solid",
                ),
                widget.Clock(
                    foreground=colors[0],
                    format="%a %d-%m-%y"
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
                widget.TextBox(
                    foreground=colors[11],
                    text=" ",
                    font="Font Awesome 5 Free Solid",
                ),
                widget.Clock(
                    foreground=colors[0],
                    format="%H:%M"
                ),
                widget.Sep(
                    linewidth=0,
                    padding=20,
                    size_percent=50,
                ),
            ],
            size=30,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False
bring_front_click = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(title=re.compile('^Attach Process *')),
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True


@hook.subscribe.client_new
def _swallow(window):
    pid = window.window.get_net_wm_pid()
    ppid = psutil.Process(pid).ppid()
    cpids = {
        c.window.get_net_wm_pid(): wid for wid, c in window.qtile.windows_map.items()
    }
    for i in range(5):
        if not ppid:
            return
        if ppid in cpids:
            parent = window.qtile.windows_map.get(cpids[ppid])
            parent.minimized = True
            window.parent = parent
            return
        ppid = psutil.Process(ppid).ppid()


@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, "parent"):
        window.parent.minimized = False

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

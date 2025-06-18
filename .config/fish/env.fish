

set -qU XDG_CONFIG_HOME; or set -Ux XDG_CONFIG_HOME $HOME/.config
set -qU XDG_DATA_HOME; or set -Ux XDG_DATA_HOME $HOME/.local/share
set -qU XDG_CACHE_HOME; or set -Ux XDG_CACHE_HOME $HOME/.cache


set -gx XINITRC $XDG_CONFIG_HOME/x11/xinitrc
set -gx NOTMUCH_CONFIG $XDG_CONFIG_HOME/notmuch-config
set -gx GTK2_RC_FILES $XDG_CONFIG_HOME/gtk-2/gtkrc-2.0
set -gx WGETRC $XDG_CONFIG_HOME/wget/wgetrc
set -gx INPUTRC $XDG_CONFIG_HOME/shell/inputrc
set -gx PASSWORD_STORE_DIR $XDG_CONFIG_HOME/password-store
set -gx TMUX_TMPDIR $XDG_RUNTIME_DIR
set -gx CARGO_HOME $XDG_DATA_HOME/cargo
set -gx GOPATH $XDG_DATA_HOME/go
set -gx GOMODCACHE $XDG_DATA_HOME/go/mod
set -gx ANSIBLE_CONFIG $XDG_CONFIG_HOME/ansible/ansible.cfg
set -gx UNISON $XDG_CONFIG_HOME/unison
set -gx HISTFILE $XDG_CONFIG_HOME/history
set -gx MBSYNCRC $XDG_CONFIG_HOME/mbsync/config
set -gx ELECTRUMDIR $XDG_CONFIG_HOME/electrum
set -gx PYTHONSTARTUP $XDG_CONFIG_HOME/python/pythonrc
set -gx RIPGREP_CONFIG_PATH $XDG_CONFIG_HOME/ripgrep/ripgreprc
set -gx SQLITE_HISTORY $XDG_DATA_HOME/sqlite_history

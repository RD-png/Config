if status is-interactive
    # Commands to run in interactive sessions can go here
end

source $__fish_config_dir/env.fish
fzf --fish | source

set fish_greeting
set TERM "xterm-256color"
set FISH_CLIPBOARD_CMD "cat"

set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showupstream 'none'
set -g fish_prompt_pwd_dir_length 3

setenv LESS_TERMCAP_mb \e'[01;31m'
setenv LESS_TERMCAP_md \e'[01;38;5;74m'
setenv LESS_TERMCAP_me \e'[0m'
setenv LESS_TERMCAP_se \e'[0m'
setenv LESS_TERMCAP_so \e'[38;5;246m'
setenv LESS_TERMCAP_ue \e'[0m'
setenv LESS_TERMCAP_us \e'[04;38;5;146m'

function fish_prompt
	set_color yellow
	echo -n (whoami)
    set_color red
	echo -n ' @ '
	if [ $PWD != $HOME ]
		set_color green
		echo -n (basename $PWD)
    else
        set_color green
        echo -n '~'
	end
	set_color red
	echo -n ' % '
	set_color normal
end

function __delete-line
   commandline -f beginning-of-line
   commandline -f kill-line
end

function __history_previous_command
  switch (commandline -t)
  case "!"
    commandline -t $history[1]; commandline -f repaint
  case "*"
    commandline -i !
  end
end

function __history_previous_command_arguments
  switch (commandline -t)
  case "!"
    commandline -t ""
    commandline -f history-token-search-backward
  case "*"
    commandline -i '$'
  end
end

function __select_from_last
   set -l FZF_OUT (eval $history[1] | fzf --multi)
   if test -n "$FZF_OUT"
     commandline -r $FZF_OUT
     commandline --cursor 0
     fish_clipboard_copy
     commandline (commandline --cut-at-cursor)
     commandline -f repaint
   end
end

function fish_clipboard_copy
	if type -q pbcopy
        commandline | pbcopy
    else if type -q xsel
        commandline | xsel --clipboard
    end
end

# Custom binds
bind \cg cancel
bind \cH backward-kill-word
bind \cw __delete-line
bind \er __select_from_last
bind \ew fish_clipboard_copy
bind \ey fish_clipboard_paste
bind ! __history_previous_command
bind '$' __history_previous_command_arguments
bind \ct transpose-chars
bind \et transpose-words
bind \ec upcase-word

alias ls "ls --group-directories-first --color -l"
alias lsd "ls -d */"
alias lsa "ls -a"
alias cp "cp -i"
alias mv "mv -i"
alias rm "rm -i"
alias diff "diff --color=auto"
alias cdd "fzf-cd-widget -e --tiebreak=length"
alias cdf "fzf-file-widget -e"

# bind \t accept-autosuggestion
# bind \t\t complete

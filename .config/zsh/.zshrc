# Created by newuser for 5.8
autoload -U colors && colors
PS1="%B%{$fg[magenta]%}%n%{$fg[blue]%} @ %{$fg[yellow]%}%M %{$fg[green]%}%~ %{$reset_color%}#%b "
stty stop undef
eval "$(direnv hook zsh)"

export GOPATH="$(go env GOPATH)"
export PATH="${PATH}:${GOPATH}/bin"

init_flake() {
  if [ ! -e flake.nix ]; then
    nix flake new -t github:nix-community/nix-direnv .
  elif [ ! -e .envrc ]; then
    echo "use flake" > .envrc
    direnv allow    
  fi

  git init
  git add flake.nix
}

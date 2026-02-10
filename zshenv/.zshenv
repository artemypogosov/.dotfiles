#################
### VARIABLES ###
#################

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export LOCAL_BIN="$HOME/.local/bin"

export ZDOTDIR="$HOME/.config/zsh"
export XMONAD_CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/xmonad"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export LEIN_HOME="$XDG_DATA_HOME/lein"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export SCREENRC="$XDG_CONFIG_HOME/screen/screenrc"
export ZDOTDIR="$HOME/.config/zsh"

# Disable persistent history (for safety reasons)
export LESSHISTFILE=-
export MYSQL_HISTFILE=/dev/null
export PSQL_HISTORY=/dev/null

export XCURSOR_THEME="Adwaita"
export EDITOR=nvim
export VISUAL=nvim

# Cutefish
export CF_TITLE=false

export LC_TIME="en_US.UTF-8"

# GO clean XDG paths
export GOPATH="$XDG_DATA_HOME/go"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"

export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc"

export PATH="$GOBIN:$XDG_CONFIG_HOME/emacs/bin:$PATH"

[ -f "$HOME/.dotfiles/.secrets/.zshenv.local" ] && source "$HOME/.dotfiles/.secrets/.zshenv.local"

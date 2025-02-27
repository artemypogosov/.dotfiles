# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Cutefish
~/.local/bin/cutefetch random

# -- Aliases
alias ls='ls --color=auto --hyperlink=auto "$@"'
alias ll='ls -la'
alias ls.="ls -A | grep '^\.'"
alias grep='grep --color=auto'
alias df='df -h'
alias free="free -th"
alias userlist="cut -d: -f1 /etc/passwd"
alias jctl="journalctl -p 3 -xb"
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"

# Passing two --refresh or -y flags will force a refresh of all package lists
# even if they appear to be up to date.
alias update='sudo pacman -Syyu'
# Cleanup orphaned packages
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

# Add new fonts
alias update-fc='sudo fc-cache -fv'

# Recently installed packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -25 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -100 | nl"

# -- History
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# -- Auto-complition
autoload -Uz compinit
zstyle ':completion:*' menu select
# Case insensitive complition
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zmodload zsh/complist
compinit
# Include hidden files
_comp_options+=(globdots)

# -- Use 'vim' mode
bindkey -v
export KEYTIMEOUT=1
# Press ctrl+e to edit command in editor
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# -- Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
 # Initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
   zle -K viins
   echo -ne "\e[5 q"
}
zle -N zle-line-init
# Use beam shape cursor on startup.
echo -ne '\e[5 q'
# Use beam shape cursor for each new prompt.
preexec() { echo -ne '\e[5 q' ;}

# -- Use 'ranger' to switch directories and bind it to ctrl-r
rangercd () {
    tmp="$(mktemp)"
    ranger --choosedir="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^r' 'rangercd\n'

# -- Add availability of local scripts right from the shell
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

POWERLEVEL9K_CONFIG_FILE=~/.config/zsh/p10k.zsh

# -- Plugins
source ~/.dotfiles/.config/zsh/powerlevel10k/powerlevel10k.zsh-theme
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# -- Disabling suggestion for large buffers
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

bindkey '^ ' autosuggest-accept
bindkey '^t' autosuggest-toggle

# To customize prompt, run `p10k configure` or edit ~/p10k.zsh.
[[ ! -f ~/.config/zsh/p10k.zsh ]] || source ~/.config/zsh/p10k.zsh

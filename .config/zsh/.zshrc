# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

~/.local/bin/cutefetch random


#export LC_TIME="en_US.UTF-8"

###############
### ALIASES ###
###############
alias ..='cd ..'
alias ...='cd ../..'
alias rm='rm -I'
alias cp='cp -vi'
alias ls='ls -h --color=auto --hyperlink=auto "$@"'
alias ll='ls -lah'
alias ls.="ls -A | grep '^\.'"
alias grep='grep --color=auto'
alias df='df -h'
alias free='free -th'
alias userlist='cut -d: -f1 /etc/passwd'
alias jctl='journalctl -p 3 -xb'
alias psgrep='ps aux | grep -v grep | grep -i -e VSZ -e'
alias wget="wget --hsts-file='$XDG_CACHE_HOME/wget-hsts'"

alias vim='nvim'
alias emacs="emacsclient -c -a 'emacs'"

# Test webcam
alias webcam='ffplay /dev/video0'

# Change bash default init file
alias bash="bash --init-file ~/.local/state/bash/.bashrc"

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

# fzf
alias vf='vim $(find . -type f | fzf)'
alias fzf='find . -type f | fzf'

# Rick Astley
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

###############
### HISTORY ###
###############
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

#######################
### AUTO COMPLETION ###
#######################
autoload -Uz compinit
zstyle ':completion:*' menu select
# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zmodload zsh/complist
compinit
# Include hidden files
_comp_options+=(globdots)

#######################
### ENABLE VIM MODE ###
#######################
bindkey -v
export KEYTIMEOUT=1
# Press ctrl+e to edit command in editor
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

############################################
### CURSOR SHAPE FOR DIFFERENT VIM MODES ###
############################################
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

########################
### CUSTOM FUNCTIONS ###
########################
function ex {
 if [ -z "$1" ]; then
    # display usage if no parameters given
    echo "Usage: ex <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
 else
    for n in "$@"
    do
      if [ -f "$n" ] ; then
          case "${n%,}" in
            *.cbt|*.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar)
                         tar xvf "$n"       ;;
            *.lzma)      unlzma ./"$n"      ;;
            *.bz2)       bunzip2 ./"$n"     ;;
            *.cbr|*.rar)       unrar x -ad ./"$n" ;;
            *.gz)        gunzip ./"$n"      ;;
            *.cbz|*.epub|*.zip)       unzip ./"$n"       ;;
            *.z)         uncompress ./"$n"  ;;
            *.7z|*.arj|*.cab|*.cb7|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.pkg|*.rpm|*.udf|*.wim|*.xar)
                         7z x ./"$n"        ;;
            *.xz)        unxz ./"$n"        ;;
            *.exe)       cabextract ./"$n"  ;;
            *.cpio)      cpio -id < ./"$n"  ;;
            *.cba|*.ace)      unace x ./"$n"      ;;
            *)
                         echo "ex: '$n' - unknown archive method"
                         return 1
                         ;;
          esac
      else
          echo "'$n' - file does not exist"
          return 1
      fi
    done
fi
}



################################################################
### USE 'RANGER' TO SWITCH DIRECTORIES AND BIND IT TO CTRL-R ###
################################################################
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

##############################################################
### ADD AVAILABILITY OF LOCAL SCRIPTS RIGHT FROM THE SHELL ###
##############################################################
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

POWERLEVEL9K_CONFIG_FILE=~/.config/zsh/p10k.zsh

###############
### PLUGINS ###
###############
source ~/.dotfiles/.config/zsh/powerlevel10k/powerlevel10k.zsh-theme
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Disabling suggestion for large buffers
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

# Toggle autosuggestion via ctrl+t
bindkey '^ ' autosuggest-accept
bindkey '^t' autosuggest-toggle

# To customize prompt, run `p10k configure` or edit ~/p10k.zsh.
[[ ! -f ~/.config/zsh/p10k.zsh ]] || source ~/.config/zsh/p10k.zsh

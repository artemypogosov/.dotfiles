# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# -- Set variables
export HISTCONTROL=ignoreboth:erasedups
export HISTFILE="$XDG_STATE_HOME"/bash/history
export EDITOR='emacs'
export VISUAL='emacs'
# Doom emacs [use 'doom <command>' to run commands]
export PATH="$HOME/.emacs.d/bin:$PATH"
# Cutefix
export CF_TITLE=false
~/.local/bin/cutefetch random

# -- Shell prompt template
PS1='[\u@\h \W]\$ '

if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

# -- Ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

# -- Aliases
alias ls='ls --color=auto --hyperlink=auto "$@"'
alias ll='ls -la'
alias ls.="ls -A | grep '^\.'"
alias grep='grep --color=auto'
alias df='df -h'
alias free="free -th"
alias userlist="cut -d: -f1 /etc/passwd"

# Aliases for software managment
# Passing two -y flags will force a refresh of all package lists even if they appear to be up to date.
alias update='sudo pacman -Syyu'

# Cleanup orphaned packages
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"

# Add new fonts
alias update-fc='sudo fc-cache -fv'

# Switch between bash and zsh
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"

# Hardware info --short
alias hw="hwinfo --short"

# Mounting the folder Public for exchange between host and guest on virtualbox
alias vbm="sudo /usr/local/bin/arcolinux-vbox-share"

# -- Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

# -- Search content with ripgrep
alias rg="rg --sort path"

# -- Get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# -- Shopt
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases


# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

#!/bin/bash
set -euo pipefail

cat other/computer.txt
echo
echo "Date: $(date +"%a %b %d %H:%M:%S")"
echo
echo "The recommendation is to run setup.sh from an active XMonad instance."
echo

read -r -p "Start setup? (y/n)? " choice
case "$choice" in
y | Y)
  echo "======================"
  echo "=== Starting setup ==="
  echo "======================"

  echo "=== Creating directories ==="
  mkdir -p \
    ~/.config/simplescreenrecorder \
    ~/.config/X11 \
    ~/.config/auth \
    ~/.config/wget-hsts \
    ~/.config/screen \
    ~/.config/git \
    ~/.config/npm/config \
    ~/.config/docker \
    ~/.local/share/lein \
    ~/.local/share/cargo \
    ~/.cache/mypy \
    ~/.local/state/npm/logs \
    ~/.local/state/bash

  echo "=== Installing programs... [pacman + yay] ==="
  pacman -S - <~/.dotfiles/other/pacman-pkgs.txt
  yay -S - <~/.dotfiles/other/yay-pkgs.txt

  echo "=== GNU Stow: linking .dotfiles ==="
  stow .

  echo "=== Downloading .etckeeper repo ==="
  git clone https://github.com/artemypogosov/etckeeper ~/.config/etckeeper &&
    systemctl enable lightdm.service &&
    echo "=== Downloading wallpapers ==="
  if [ ! -d ~/Pictures/wallpapers ]; then
    git clone https://github.com/artemypogosov/wallpapers ~/Pictures/wallpapers
  fi

  echo "=== Setup wallpapers ==="
  systemctl enable "betterlockscreen@$USER" &&
    read -r -p "Are you running a dual-monitor setup? (y/n)? " choice
  case "$choice" in
  y | Y)
    nitrogen --set-zoom-fill ~/Pictures/wallpapers/dual-monitor/stray.png
    betterlockscreen -u ~/Pictures/wallpapers/dual-monitor/stray.png --span --display 2
    ;;
  n | N)
    nitrogen --set-zoom-fill ~/Pictures/wallpapers/stray.png
    betterlockscreen -u ~/Pictures/wallpapers/stray.png
    ;;
  *)
    echo "Please answer 'yes' or 'no'"
    ;;
  esac

  echo "=== Setup shell ==="
  chsh -s /bin/zsh &&

    # Setup cursor theme
    xrdb -load ~/.config/X11/.Xresources

  if [ ! -d ~/.cache/zsh ]; then
    mkdir -p ~/.cache/zsh
    touch ~/.cache/zsh/history
  fi

  if [ ! -d ~/.local/share/applications ]; then
    mkdir -p ~/.local/share/applications
    cp /usr/share/applications/kitty.desktop ~/.local/share/applications/
  fi

  rm ~/.bashrc
  cp ~/.bash* ~/.local/state/bash

  echo "================================"
  echo "=== RECOMMENDED MANUAL STEPS ==="
  echo "================================"

  echo "1. Add '--lock dimblur' and optional '--display 2' in /usr/lib/systemd/system/betterlockscreen@.service"
  echo "2. Set 'kitty.desktop' to NoDisplay=true in .local/share/applications/"
  echo "3. cp -a ~/.config/etckeeper/lightdm/. /etc/lightdm/"
  echo "4. Open 'lightdm-gtk-greeter-settings' and set background depending on [PC or laptop]"
  echo "5. Setup powermanagement [suspend on lid close, look at tray power icon]"
  echo "6. cp ~/.dotfiles/other/.pws ~/.emacs.d/.local/etc/ispell/.pws  (.pws - dictionary file for Emacs (syntax +aspell))"
  echo "7. Setup syncthing"
  sleep 3
  echo "=== Ready! ==="
  ;;
*)
  echo "Exit."
  exit 1
  ;;
esac

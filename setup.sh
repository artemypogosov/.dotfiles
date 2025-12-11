#!/bin/bash
set -euo pipefail

# '&&' at the end means "run next command only if the previous command succeeded"

#############
# VARIABLES #
#############

SETUP_DIR="$HOME/.dotfiles/setup"

RED="\033[31m"
GREEN="\033[32m"
BLUE="\033[34m"
RESET="\033[0m"

###################
# SUDO KEEP-ALIVE #
###################

# Ask for sudo password upfront
sudo -v

# Keep sudo alive for the whole duration of the script
# (prevents repeated password prompts)
while true; do
    sudo -n true
    sleep 60
done 2>/dev/null &
SUDO_KEEPALIVE=$!

#############
# FUNCTIONS #
#############

# Install packages from a list file.
# Each file can contain:
#   - package names
#   - blank lines
#   - comments (# ...)
#
# Behavior:
#   - Installs via pacman if available
#   - Otherwise falls back to yay (AUR)
#   - Logs missing packages without aborting

install_file() {
    local file="$1"

    echo -e ">>> Installing packages from: ${BLUE}$file${RESET}"

    while IFS= read -r pkg; do
        # Skip empty lines and comments
        [[ -z "$pkg" ]] && continue
        [[ "$pkg" =~ ^# ]] && continue

        printf "  * %-30s " "$pkg"

        # Redirect stdin so pacman/yay cannot steal it
        if pacman -Si "$pkg" </dev/null >/dev/null 2>&1; then
            if sudo pacman -S --noconfirm --needed "$pkg" </dev/null >/dev/null 2>&1; then
                echo -e "${GREEN}OK${RESET}"
            else
                echo -e "${RED}FAILED (PACMAN)${RESET}"
            fi

        else
            # Try AUR
            if yay -Si "$pkg" </dev/null >/dev/null 2>&1; then
                if yay -S --noconfirm --needed "$pkg" </dev/null >/dev/null 2>&1; then
                    echo -e "${GREEN}OK (AUR)${RESET}"
                else
                    echo -e "${RED}FAILED (AUR)${RESET}"
                fi
            else
                echo -e "${RED}NOT FOUND${RESET}"
            fi
        fi

    done <"$file"

    echo
}

###########
# STARTUP #
###########

cat other/computer.txt
echo
echo "Date: $(date +"%a %b %d %H:%M:%S")"
echo
echo "It is recommended to run this script from an active XMonad session with Chaoric AUR already configured."
echo

read -r -p "Start setup? (y/n)? " choice
case "$choice" in
    y | Y)
        echo "======================"
        echo "=== STARTING SETUP ==="
        echo "======================"

        echo "=== CREATING DIRECTORIES ==="

        xdg-user-dirs-update

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

        echo "=== INSTALLING PROGRAMS... [PACMAN + AUR] ==="

        install_file "$SETUP_DIR/pacman-pkgs.txt"
        install_file "$SETUP_DIR/pacman-lsp.txt"
        install_file "$SETUP_DIR/pacman-formatters.txt"
        install_file "$SETUP_DIR/yay-pkgs.txt"
        install_file "$SETUP_DIR/yay-formatters.txt"

        echo "=== GNU STOW: LINKING .DOTFILES ==="

        cd ~/.dotfiles
        stow .

        echo "=== DOWNLOADING .etckeeper REPO ==="

        if [[ ! -d "$HOME/.config/etckeeper" ]]; then
            git clone https://github.com/artemypogosov/etckeeper "$HOME/.config/etckeeper"
        fi

        sudo systemctl enable lightdm.service

        echo "=== DOWNLOADING WALLPAPERS ==="

        if [[ ! -d "$HOME/Pictures/wallpapers" ]]; then
            git clone https://github.com/artemypogosov/wallpapers "$HOME/Pictures/wallpapers"
        fi

        echo "=== SETUP WALLPAPERS ==="

        sudo systemctl enable --now "betterlockscreen@$(whoami).service"

        if pgrep -x Xorg >/dev/null; then
            read -r -p "Are you running a dual-monitor setup? (y/n)? " dm_choice
            case "$dm_choice" in
                y | Y)
                    nitrogen --set-zoom-fill "$HOME/Pictures/wallpapers/dual-monitor/stray.png"
                    betterlockscreen -u "$HOME/Pictures/wallpapers/dual-monitor/stray.png" --span --display 2
                    ;;
                n | N)
                    nitrogen --set-zoom-fill "$HOME/Pictures/wallpapers/stray.png"
                    betterlockscreen -u "$HOME/Pictures/wallpapers/stray.png"
                    ;;
                *)
                    echo "Invalid input. Skipping wallpaper setup."
                    ;;
            esac
        fi

        echo "=== SETUP SHELL ==="

        mkdir -p ~/.cache/zsh
        touch ~/.cache/zsh/history

        chsh -s /bin/zsh

        echo "=== DISABLE KITTY FROM LAUNCHER ==="
        mkdir -p ~/.local/share/applications

        if [[ -f /usr/share/applications/kitty.desktop ]]; then
            cp /usr/share/applications/kitty.desktop ~/.local/share/applications/
            echo NoDisplay=true >>~/.local/share/applications/kitty.desktop
        fi

        echo "=== MOVE BASH to ~/.local/state/bash ==="
        mv ~/.bash* ~/.local/state/bash 2>/dev/null || true

        echo "=== COPY LIGHTDM CONFIG FILES INTO /etc/lightdm/ ==="
        sudo cp -a ~/.config/etckeeper/lightdm/. /etc/lightdm/

        echo "================================"
        echo "=== RECOMMENDED MANUAL STEPS ==="
        echo "================================"

        cat <<EOF
1. Add '--lock dimblur' (and optionally '--display 2') in: /usr/lib/systemd/system/betterlockscreen@.service

2. Run 'lightdm-gtk-greeter-settings' and configure background

3. Setup power management via applet icon (lid close suspend, etc.)

4. Copy dictionary for Emacs (DOOM MUST BE ALREADY INSTALLED) : cp ~/.dotfiles/other/.pws $HOME/.config/emacs/.local/etc/ispell/.pws

5. Install dockfmt: go install github.com/jessfraz/dockfmt@latest
Then move ~/go into ~/.config/go using XDG BASE DIRECTORY: https://wiki.archlinux.org/title/XDG_Base_Directory
Then add new path to $PATH in .zshenv

6. Configure Syncthing [sudo systemctl enable syncthing@service --now]
EOF

        sleep 3

        ########################
        # STOP SUDO KEEP-ALIVE #
        ########################

        kill "$SUDO_KEEPALIVE" 2>/dev/null || true

        echo "=== READY! ==="
        ;;
    *)

        echo "Exit."

        exit 1
        ;;
esac

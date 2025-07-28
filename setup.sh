#!/usr/bin/env sh

cat computer.txt
echo &&

while true; do
    read -p "Start setup? (y/n)? " choice
    case "$choice" in
        y|Y )
            echo "======================" &&
            echo "=== Starting setup ===" &&
            echo "======================" &&

            echo "=== Installing programms... [pacman + yay] ===" &&
            pacman -S - < ~/.dotfiles/packages-list/pacman-pkgs.txt &&
            yay -S - < ~/.dotfiles/packages-list/yay-pkgs.txt &&

            echo "=== Downloading .etckeeper repo ===" &&
            git clone https://github.com/artemypogosov/.etckeeper ~/ &&
            cp ~/.etckeeper/lightdm/* /etc/lightdm
            systemctl enable lightdm.service

            echo "=== Downloading wallpapers ===" &&
            git clone https://github.com/artemypogosov/wallpapers ~/Pictures/wallpapers &&

            echo "=== Setup wallpapers ===" &&
            systemctl enable betterlockscreen@$USER &&

            while true; do
            read -p "Are you running a dual-monitor setup? (y/n)? " choice
            case "$choice" in
                y|Y )
                    nitrogen --set-auto ~/Pictures/wallpapers/dual-monitor/stray.png
                    betterlockscreen -u ~/Pictures/wallpapers/dual-monitor/stray.png --span --display 3;;
                n|N )
                    nitrogen --set-auto ~/Pictures/wallpapers/stray.png
                    betterlockscreen -u ~/Pictures/wallpapers/stray.png;;
                * )
                    echo "Please answer 'yes' or 'no'";;
            esac
            done

            echo "=== Setup shell ==="
            chsh -s /bin/zsh &&
            mkdir .cache/zsh &&
            touch .cache/zsh/history &&

            cp /usr/share/applications/kitty.desktop .local/share/applications/ &&

            echo "================================" &&
            echo "=== RECOMMENDED MANUAL STEPS ===" &&

            echo "1. Add '--lock dimblur' in /usr/lib/systemd/system/betterlockscreen@.service" &&
            echo "2. Set 'kitty.desktop' to NoDisplay=true in .local/share/applications/" &&

            echo "================================" &&

            echo "=== Ready! ===";;
        * )
            echo "Exit."
            exit 1;;
    esac
done

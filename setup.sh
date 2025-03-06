#!/usr/bin/env sh

cat computer.txt
echo "It's better to run setup.sh from active XMonad instance, make sure to run 'stow .' in .dotfiles."
echo 

    read -p "Start setup? (y/n)? " choice
    case "$choice" in
        y|Y )
            echo "======================" 
            echo "=== Starting setup ===" 
            echo "======================" 

            echo "=== Installing programms... [pacman + yay] ===" 
            pacman -S - < packages-list/pacman-pkgs.txt 
	    yay -S - < packages-list/yay-pkgs.txt

            echo "=== Downloading .etckeeper repo ==="
            git clone https://github.com/artemypogosov/.etckeeper ~/.etckeeper &&
	    systemctl enable lightdm.service &&

            echo "=== Downloading wallpapers ===" 
  	    if [ ! -d ~/Pictures/wallpapers ]; then
	    	git clone https://github.com/artemypogosov/wallpapers ~/Pictures/wallpapers 
	    fi

            echo "=== Setup wallpapers ===" 
            systemctl enable betterlockscreen@$USER &&

            read -p "Are you running a dual-monitor setup? (y/n)? " choice
            case "$choice" in
                y|Y )
                    nitrogen --set-zoom-fill ~/Pictures/wallpapers/dual-monitor/stray.png
                    betterlockscreen -u ~/Pictures/wallpapers/dual-monitor/stray.png --span --display 3;;
                n|N )
                    nitrogen --set-zoom-fill ~/Pictures/wallpapers/stray.png
                    betterlockscreen -u ~/Pictures/wallpapers/stray.png;;
                * )
                    echo "Please answer 'yes' or 'no'";;
            esac

            echo "=== Setup shell ==="
            chsh -s /bin/zsh &&
	    
  	    if [ ! -d ~/.cache/zsh ]; then
            	mkdir ~/.cache/zsh 
            	touch ~/.cache/zsh/history 
	    fi

  	    if [ ! -d ~/.local/share/applications ]; then
	    	mkdir ~/.local/share/applications 
            	cp /usr/share/applications/kitty.desktop ~/.local/share/applications/
	    fi

            echo "================================" 
            echo "=== RECOMMENDED MANUAL STEPS ===" 
            echo "================================" 

            echo "1. Add '--lock dimblur' in /usr/lib/systemd/system/betterlockscreen@.service" 
            echo "2. Set 'kitty.desktop' to NoDisplay=true in .local/share/applications/" 
	        echo "3. cp -a ~/.etckeeper/lightdm/. /etc/lightdm/"
	        echo "4. Open 'lightdm-gtk-greeter-settings' and set background depending on [PC or laptop]"
            echo "5. Setup powermanagement [suspend on lid close, look at tray power icon]"
            sleep 3
            echo "=== Ready! ===";;
        * )
            echo "Exit."
            exit 1;;
    esac

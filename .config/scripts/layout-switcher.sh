#!/bin/sh

# If an explicit layout is provided as an argument, use it. Otherwise, select the next layout from
# the set [us, ua].
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | grep layout | awk '{print $2}')
    case $layout in
        us)
            setxkbmap ua ;;
        *)
            setxkbmap us ;;
    esac
fi

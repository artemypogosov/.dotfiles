#!/bin/sh

# Replace capslock with esc
setxkbmap -option
setxkbmap -option caps:escape &

# Increase typing rate
xset r rate 400 50

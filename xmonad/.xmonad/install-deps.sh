#!/bin/sh
# Puts xmobar/xmonad in ~/.local/bin
stack install xmobar \
      --flag xmobar:with_xpm \
      --flag xmobar:with_weather \
      --flag xmobar:with_xrender \
      --flag xmobar:with_threaded \
      --flag xmobar:with_alsa
sudo pacman -S xfce4-power-manager dunst xorg-xmessage \
     xss-lock xscreenserver xorg-xbacklight rofi trayer \
     gnome-terminal dmenu

stack install xmonad

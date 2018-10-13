#!/bin/sh
# Puts xmobar/xmonad in ~/.local/bin
stack install xmobar \
      --flag xmobar:with_xft \
      --flag xmobar:with_xpm \
      --flag xmobar:with_weather \
      --flag xmobar:with_alsa \
      --flag xmobar:with_utf8
stack install xmonad

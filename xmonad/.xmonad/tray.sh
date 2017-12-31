#!/bin/bash

# Launch trayer to take up the remainder that will be left by xmobar
IFS=' ' geom=($(cat ~/.config/xmobar/xmobarrc | grep TopSize \
            | sed -E 's;.*TopSize L\s+([0-9]+)\s+([0-9]+).*;\1 \2;g'))
widthPercent=${geom[0]}
heightPx=${geom[1]}
echo "width is $widthPercent and height is $heightPx"
if [[ "$widthPercent" -gt 0 ]] && \
       [[ "$widthPercent" -lt 100 ]] && \
       [[ "$heightPx" -gt 0 ]]; then
    trayerWidth=$(expr 100 - $widthPercent)
    # -l flag has trayer lower itself on start so the tray is under
    # all other windows when we toggle struts. Flag was added in trayer 1.7
    trayer --edge top --align right \
           -l \
           --width $trayerWidth --widthtype percent \
           --height $heightPx --heighttype pixel \
           --transparent true \
           --alpha 0 \
           --tint 0x000000
fi

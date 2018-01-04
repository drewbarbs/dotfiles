#!/bin/bash

IFS=' ' geom=($(runhaskell ~/dotfiles/xmonad/.xmonad/getTrayerGeom.hs))

if [[ "$?" -eq 0 ]]; then
    # cleanup any existing instances
    killall trayer

    monitor=${geom[0]}
    widthPx=${geom[1]}
    heightPx=${geom[2]}

    # -l flag has trayer lower itself on start so the tray is under
    # all other windows when we toggle struts. Flag was added in trayer 1.7
    trayer --edge top --align right \
           --monitor $monitor \
           -l \
           --iconspacing 10 \
           --widthtype pixel --width $widthPx \
           --heighttype pixel --height $heightPx \
           --transparent true \
           --alpha 0 \
           --tint 0x000000
fi

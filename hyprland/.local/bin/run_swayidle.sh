#!/bin/bash

swayidle -w \
         timeout  900 'swaylock -f -c 000000' \
         timeout 1200 'hyprctl dispatch dpms off' \
               resume 'hyprctl dispatch dpms on'  \
         timeout 1500 'systemctl suspend'

#!/bin/bash

zenity --question --text "Exit hyprland session?" && hyprctl dispatch exit

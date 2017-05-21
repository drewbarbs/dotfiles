#!/bin/bash

# Configure gnome-terminal with
# gconftool --type string --set /apps/gnome-terminal/profiles/Default/custom_command "$HOME/launch-tmux.sh"
# gconftool --type bool --set /apps/gnome-terminal/profiles/Default/use_custom_command "true"
# or, on newer gnome
# PROFILE_ID=$(dconf read /org/gnome/terminal/legacy/profiles:/default | tr -d \')
# gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$PROFILE_ID/ custom-command "$HOME/launch-tmux.sh"
# gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$PROFILE_ID/ use-custom-command true

! tmux list-sessions && tmux -2 new-session -A -s main || (_trap_exit() { tmux kill-session -t $$; }; trap _trap_exit EXIT; tmux -2 new-session -s $$ -t main \; new-window)

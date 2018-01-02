#!/bin/bash

trap "exit" INT TERM
trap "kill 0" EXIT

print_speaker() {
    if [[ "$1" == "on" ]]; then
        echo -n "<fc=#CEFFAC><icon=spkr_01.xbm/></fc>"
    else
        echo -n "<fc=#FFB6B0><icon=spkr_mute.xbm/></fc>"
    fi
}

update_volume() {
    IFS=' ' status=($(amixer get Master | awk -F"[][%]" '/Front Left:/ { print $2, $5 }'))
    vol=${status[0]}
    state=${status[1]}
    print_speaker $state
    echo " $vol%";
}

update_volume
while read -r e; do
    update_volume
done < <(pactl subscribe | grep --line-buffered sink)

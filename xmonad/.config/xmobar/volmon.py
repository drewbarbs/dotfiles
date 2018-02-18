#!/usr/bin/env python3
import subprocess
import threading

from stdout_watch import watch_for_stdout_close

def proc_lines(p):
    while True:
        line = p.stdout.readline().decode('utf8')
        if line == '':
            break
        yield line

def update_volume():
    vol, state = subprocess.check_output("amixer get Master | awk -F'[][%]' '/Front Left:/ { print $2, $5 }'",
                                         shell=True).decode('utf8').split()
    if state == 'on':
        print('<fc=#CEFFAC><icon=spkr_01.xbm/></fc>', end='')
    else:
        print('<fc=#FFB6B0><icon=spkr_mute.xbm/></fc>', end='')

    print(' {}%'.format(vol), flush=True)

update_volume()
p = subprocess.Popen(['pactl', 'subscribe'], stdout=subprocess.PIPE)

stdout_close_watcher = threading.Thread(target=watch_for_stdout_close, args=(lambda: p.kill(),))
stdout_close_watcher.deamon = True
stdout_close_watcher.start()

for l in proc_lines(p):
    if 'sink' in l:
        update_volume()

#!/usr/bin/env python3
from subprocess import Popen, PIPE, call
from time import sleep

p = Popen(['/usr/bin/xscreensaver-command', '-watch'],
          stdout=PIPE)

def proc_lines(p):
    while True:
        line = p.stdout.readline().decode('utf8')
        if line == '':
            break
        yield line

for l in proc_lines(p):
    if l.startswith('LOCK') or l.startswith('BLANK'):
        call(['killall', '-SIGUSR1', 'dunst'])
    elif l.startswith('UNBLANK'):
        sleep(1)
        call(['killall', '-SIGUSR2', 'dunst'])

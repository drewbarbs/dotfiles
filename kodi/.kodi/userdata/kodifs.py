#!/usr/bin/env python
import shlex
import subprocess
WIN_ID_CMD = shlex.split('xdotool search --class Kodi')
win_id = subprocess.check_output(WIN_ID_CMD).decode('utf8').strip()
subprocess.call(shlex.split('wmctrl -i -r {} -b toggle,fullscreen'.format(win_id)))

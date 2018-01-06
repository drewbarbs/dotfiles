#!/usr/bin/env python

# A script to poll for available arch updates every hour, inspired by
# the "arch-update" Gnome-Shell extension:
# https://github.com/RaphaelRochet/arch-update

import atexit
import select
import subprocess
import sys
from datetime import timedelta
from threading import Lock, Thread, Timer

import gi
gi.require_version('GLib', '2.0')
gi.require_version('Gio', '2.0')

from gi.repository import GLib
from gi.repository import Gio

from stdout_watch import watch_for_stdout_close

CHECK_LOCK = Lock()
CHECK_CMD = '/usr/bin/checkupdates'
CHECK_PERIOD = timedelta(hours=1)
PACMAN_DIR = '/var/lib/pacman/local'

def check_for_updates(*args):
    try:
        with CHECK_LOCK:
            output_bytes = subprocess.check_output(CHECK_CMD)
    except:
        print('<fc=#FFB6B0><icon=arch-error-symbolic.xbm/>ERROR</fc>')
    else:
        output = output_bytes.decode('utf8')
        if output:
            n_avail = len(output.split('\n')) - 1
            print(("<action=`bash -c 'zenity --text-info --filename=<(checkupdates)'` button=1>"
                   "<fc=#CEFFAC><icon=arch-updates-symbolic.xbm/></fc> {}</action>").format(n_avail))
        else:
            print('<icon=arch-uptodate-symbolic.xbm/>')
    sys.stdout.flush()

def start_next_timer():
    timer_thread = Timer(CHECK_PERIOD.total_seconds(), timer_func)
    timer_thread.daemon = True
    timer_thread.start()

def timer_func():
    check_for_updates()
    start_next_timer()

start_next_timer()

check_for_updates()

gfile = Gio.file_new_for_path(PACMAN_DIR)
monitor = gfile.monitor_directory(Gio.FileMonitorFlags.NONE)
monitor.connect('changed', check_for_updates)
atexit.register(lambda: monitor.cancel())

ml = GLib.MainLoop()

stdout_watcher = Thread(target=watch_for_stdout_close, args=(lambda: ml.quit(),))
stdout_watcher.daemon= True
stdout_watcher.start()

ml.run()

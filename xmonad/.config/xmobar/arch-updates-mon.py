#!/usr/bin/env python

# A script to poll for available arch updates every hour, inspired by
# the "arch-update" Gnome-Shell extension:
# https://github.com/RaphaelRochet/arch-update

import argparse
import atexit
import functools
import subprocess
import sys
from contextlib import suppress
from datetime import timedelta
from threading import Lock, Thread, Timer

import gi
gi.require_version('GLib', '2.0')
gi.require_version('Gio', '2.0')

from gi.repository import GLib
from gi.repository import Gio

from stdout_watch import watch_for_stdout_close

CHECK_LOCK = Lock()
DEFAULT_CHECK_PERIOD = timedelta(hours=1)

DEFAULT_CHECK_CMD = '/usr/bin/checkupdates'
DEFAULT_WATCH_DIR = '/var/lib/pacman/local'


def tryprint(s):
    with suppress(Exception):
        print(s)


def check_for_updates(check_cmd, *args):
    try:
        with CHECK_LOCK:
            output_bytes = subprocess.check_output(check_cmd, shell=True)
    except:
        tryprint('<fc=#FFB6B0><icon=arch-error-symbolic.xbm/>ERROR</fc>')
    else:
        output = output_bytes.decode('utf8')
        if output:
            n_avail = len(output.split('\n')) - 1
            tryprint((
                "<action=`bash -c 'zenity --text-info --filename=<(checkupdates)'` button=1>"
                "<fc=#CEFFAC><icon=arch-updates-symbolic.xbm/></fc> {}</action>"
            ).format(n_avail))
        else:
            tryprint('<icon=arch-uptodate-symbolic.xbm/>')

    with suppress(BrokenPipeError):
        sys.stdout.flush()


def start_next_timer(check_period, *args):
    timer_thread = Timer(
        check_period.total_seconds(), timer_func, args=(check_period, ) + args)
    timer_thread.daemon = True
    timer_thread.start()


def timer_func(check_period, check_cmd):
    check_for_updates(check_cmd)
    start_next_timer(check_period, check_cmd)


if __name__ == '__main__':
    p = argparse.ArgumentParser(
        description=('Watch for available Arch package updates '
                     'and print xmobar markup'))
    p.add_argument(
        '--cmd',
        help=('Command used to get available updates. '
              'Defaults to ' + DEFAULT_CHECK_CMD),
        default=DEFAULT_CHECK_CMD)
    p.add_argument(
        '--dir',
        help=('Directory to monitor for changes (and refresh update count). '
              'Defaults to ' + DEFAULT_WATCH_DIR),
        default=DEFAULT_WATCH_DIR)
    p.add_argument(
        '--interval',
        help=('Number of seconds to wait between update checks. '
              'Defaults to {}.'.format(DEFAULT_CHECK_PERIOD.total_seconds())),
        type=float,
        default=DEFAULT_CHECK_PERIOD.total_seconds())
    args = p.parse_args()

    start_next_timer(timedelta(seconds=args.interval), args.cmd)

    check_for_updates(args.cmd)

    gfile = Gio.file_new_for_path(args.dir)
    monitor = gfile.monitor_directory(Gio.FileMonitorFlags.NONE)
    monitor.connect('changed', functools.partial(check_for_updates, args.cmd))
    atexit.register(lambda: monitor.cancel())

    ml = GLib.MainLoop()

    stdout_watcher = Thread(
        target=watch_for_stdout_close, args=(lambda: ml.quit(), ))
    stdout_watcher.daemon = True
    stdout_watcher.start()

    ml.run()

    # Finally, close stdout ourselves so we can swallow exception from stdout flush
    # https://stackoverflow.com/a/18954489/756104
    with suppress(Exception):
        sys.stdout.close()

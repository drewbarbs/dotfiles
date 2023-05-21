#!/usr/bin/env python

# A script to poll for available arch updates every hour, inspired by
# the "arch-update" Gnome-Shell extension:
# https://github.com/RaphaelRochet/arch-update

import argparse
import functools
import json
import signal
import subprocess
import sys
from contextlib import suppress
from datetime import timedelta

import gi
gi.require_version('GLib', '2.0')
gi.require_version('Gio', '2.0')

from gi.repository import GLib
from gi.repository import Gio

DEFAULT_CHECK_PERIOD = timedelta(hours=1)

CHECK_CMD = '/usr/bin/checkupdates'
WATCH_DIR = '/var/lib/pacman/local'


def check_for_updates(check_cmd, *ignored):
    message = {
        # right-clicking the icon will cause us to immediately refresh
        'on-click-right': 'pkill -SIGUSR1 -f arch-updates-mon.py',
    }

    p = subprocess.run(check_cmd, capture_output=True)
    output = p.stdout.decode('utf8')
    if p.stderr:
        message['text'] = 'ERROR'
        message['tooltip'] = p.stderr.decode('utf8')
        message['alt'] = 'error'
        message['class'] = 'error'
    elif output:
        n_avail = len(output.split('\n')) - 1
        message['text'] = f'{n_avail}'
        message['tooltip'] = output
        message['alt'] = 'available'
        message['class'] = 'available'
    else:
        message['text'] = ''
        message['tooltip'] = 'Arch is up to date'
        message['alt'] = 'up-to-date'
        message['class'] = 'up-to-date'

    print(json.dumps(message), flush=True)


if __name__ == '__main__':
    p = argparse.ArgumentParser(
        description='Watch for available Arch package updates')
    p.add_argument(
        '--interval',
        help=('Number of seconds to wait between update checks. '
              'Defaults to {}.'.format(DEFAULT_CHECK_PERIOD.total_seconds())),
        type=float,
        default=DEFAULT_CHECK_PERIOD.total_seconds())
    args = p.parse_args()

    do_check = functools.partial(check_for_updates, CHECK_CMD)

    def on_filemon_evt(monitor, file, other_file, event_type, *ignored):
        # File monitor doesnt seem to emit changes_done_hint if the
        # only changes were deletes?
        if event_type in (Gio.FileMonitorEvent.CHANGES_DONE_HINT,
                          Gio.FileMonitorEvent.DELETED):
            do_check()

    ml = GLib.MainLoop()
    gfile = Gio.file_new_for_path(WATCH_DIR)
    monitor = gfile.monitor_directory(Gio.FileMonitorFlags.NONE)
    monitor.connect('changed', on_filemon_evt)

    GLib.io_add_watch(sys.stdout, GLib.PRIORITY_DEFAULT,
                      GLib.IO_HUP | GLib.IO_ERR, lambda *args: ml.quit())

    def handle_glib_event_repeat(*args):
        # GLib expects this to return True if we want this function to
        # be called repeatedly (after every timeout/signal delivery)
        do_check()
        return True

    GLib.timeout_add(1000 * args.interval, handle_glib_event_repeat)
    GLib.unix_signal_add(GLib.PRIORITY_DEFAULT, signal.SIGUSR1,
                         handle_glib_event_repeat)

    try:
        do_check()
        ml.run()
    finally:
        monitor.cancel()

    # Finally, close stdout ourselves so we can swallow exception from
    # stdout flush (since we only reach this point if the other end of
    # stdout pipe has been closed)
    # https://stackoverflow.com/a/18954489/756104
    with suppress(Exception):
        sys.stdout.close()

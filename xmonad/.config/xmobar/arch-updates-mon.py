#!/usr/bin/env python

# A script to poll for available arch updates every hour, inspired by
# the "arch-update" Gnome-Shell extension:
# https://github.com/RaphaelRochet/arch-update

import argparse
import functools
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

DEFAULT_CHECK_CMD = '/usr/bin/checkupdates'
DEFAULT_WATCH_DIR = '/var/lib/pacman/local'


def tryprint(*args, **kwargs):
    with suppress(Exception):
        print(*args, **kwargs)


def check_for_updates(check_cmd, *ignored):
    tryprint('<action=`pkill -10 -f arch-updates-mon.py` button=2>', end='')

    try:
        output_bytes = subprocess.check_output(check_cmd, shell=True)
    except:
        tryprint(
            '<fc=#FFB6B0><icon=arch-error-symbolic.xbm/>ERROR</fc>', end='')
    else:
        output = output_bytes.decode('utf8')
        if output:
            n_avail = len(output.split('\n')) - 1
            tryprint(
                ("<action=`bash -c 'zenity --text-info --filename=<(checkupdates)'` button=1>"
                 "<fc=#CEFFAC><icon=arch-updates-symbolic.xbm/></fc> {}</action>"
                 ).format(n_avail),
                end='')
        else:
            tryprint('<icon=arch-uptodate-symbolic.xbm/>', end='')

    tryprint('</action>')
    with suppress(BrokenPipeError):
        sys.stdout.flush()


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

    do_check = functools.partial(check_for_updates, args.cmd)

    def on_filemon_evt(monitor, file, other_file, event_type, *ignored):
        # File monitor doesnt seem to emit changes_done_hint if the
        # only changes were deletes?
        if event_type in (Gio.FileMonitorEvent.CHANGES_DONE_HINT,
                          Gio.FileMonitorEvent.DELETED):
            do_check()

    ml = GLib.MainLoop()
    gfile = Gio.file_new_for_path(args.dir)
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

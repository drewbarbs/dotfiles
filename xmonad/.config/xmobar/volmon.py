#!/usr/bin/env python3
import asyncio
import subprocess
import sys

from contextlib import closing


def update_volume():
    vol, state = subprocess.check_output(
        "amixer get Master | awk -F'[][%]' '/Front Left:/ { print $2, $5 }'",
        shell=True).decode('utf8').split()
    if state == 'on':
        print('<fc=#CEFFAC><icon=spkr_01.xbm/></fc>', end='')
    else:
        print('<fc=#FFB6B0><icon=spkr_mute.xbm/></fc>', end='')

    print(' {}%'.format(vol), flush=True)


class StdoutProtocol(asyncio.Protocol):
    def __init__(self, exitev):
        self.exitev = exitev

    def connection_lost(self, exc):
        # Detects stdout close *when stdout is a PIPE*
        self.exitev.set()


async def monitor(exitev):
    evtask = asyncio.create_task(exitev.wait())

    while not exitev.is_set():
        # pactl will die if pulseaudio is restarted, so we'll
        # continually relaunch (as I sometimes need to restart pulse
        # if sound gets weird)
        mon = await asyncio.create_subprocess_exec(
            'pactl',
            'subscribe',
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.DEVNULL,
            stdin=asyncio.subprocess.DEVNULL)

        while True:
            ltask = asyncio.create_task(mon.stdout.readline())
            done, pending = await asyncio.wait(
                [evtask, ltask], return_when=asyncio.FIRST_COMPLETED)
            if evtask in done:
                mon.kill()
                break
            line = ltask.result().decode('utf8')
            if not line:
                break
            if 'sink' in line:
                update_volume()

        await mon.wait()


async def main():
    loop = asyncio.get_event_loop()
    exitev = asyncio.Event()
    t, p = await loop.connect_write_pipe(lambda: StdoutProtocol(exitev),
                                         sys.stdout)
    with closing(t):
        await monitor(exitev)


update_volume()  # Initial print of volume status
asyncio.run(main())

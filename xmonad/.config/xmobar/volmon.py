#!/usr/bin/env python3
import asyncio
import subprocess
import sys


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
    def __init__(self, exit_future):
        self.exit_future = exit_future

    def connection_lost(self, exc):
        # Detects stdout close *when stdout is a PIPE*
        self.exit_future.set_result(True)


async def monitor(exit_future):
    while not exit_future.done():
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
                [exit_future, ltask], return_when=asyncio.FIRST_COMPLETED)
            if exit_future in done:
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
    exit_future = loop.create_future()
    coro = loop.connect_write_pipe(lambda: StdoutProtocol(exit_future),
                                   sys.stdout)
    stdout_watch_task = loop.create_task(coro)
    try:
        await asyncio.gather(stdout_watch_task, monitor(exit_future))
    except:
        stdout_watch_task.cancel()


update_volume()  # Initial print of volume status
asyncio.run(main())

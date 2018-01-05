import select
import sys

def watch_for_stdout_close(action=None):
    poller = select.poll()
    poller.register(sys.stdout, select.POLLERR | select.POLLHUP)
    poller.poll()
    if action:
        action()

if __name__ == '__main__':
    watch_for_stdout_close()

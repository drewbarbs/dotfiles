import contextlib
import os
import subprocess
import tempfile
import time
import unittest

TEST_FILE = './arch-updates-mon.py'


@contextlib.contextmanager
def kill(p):
    try:
        yield
    finally:
        with contextlib.suppress(Exception):
            p.kill()
        p.wait()


class TestStdoutWatch(unittest.TestCase):
    PIPES = {'std' + fd: subprocess.PIPE for fd in ('in', 'out', 'err')}

    def test_exit_on_stdout_close(self):
        """Monitor exits when the other end of stdout closes"""

        # Give the monitor a nonsense command to run
        p = subprocess.Popen([TEST_FILE, '--cmd', 'echo something'],
                             **self.PIPES)
        with kill(p):
            with self.assertRaises(subprocess.TimeoutExpired):
                p.wait(0.5)
            p.stdout.close()

            # The process should finish within two seconds
            self.assertEqual(0, p.wait(2.0))

            p.stdin.close()
            p.stderr.close()

    def test_timeout_trigger(self):
        """ Monitor checks for updates after every --interval seconds """

        with tempfile.TemporaryDirectory() as dname:
            # Give a command we can observe result of
            checklog = os.path.join(dname, 'checklog')
            cmd = 'echo something >> {}'.format(checklog)

            p = subprocess.Popen([TEST_FILE, '--cmd', cmd, '--interval=0.1'],
                                 **self.PIPES)
            with kill(p):
                time.sleep(0.5)
                p.stdout.close()

                self.assertEqual(0, p.wait(1.0))
                p.stdin.close()
                p.stderr.close()

            with open(checklog, 'rb') as f:
                self.assertGreaterEqual(len(f.readlines()), 5)

    def test_dir_change_trigger(self):
        """ Monitor checks for updates when the monitored directory changes """

        with tempfile.TemporaryDirectory() as monitor_dname,\
               tempfile.TemporaryDirectory() as tmpdir:
            checklog = os.path.join(tmpdir, 'checklog')
            cmd = 'echo something >> {}'.format(checklog)

            monitored_file = os.path.join(monitor_dname, 'monitor')
            # Prestage file
            with open(monitored_file, 'wb') as f:
                f.write(b'a')

            p = subprocess.Popen([
                TEST_FILE, '--cmd', cmd, '--interval=10000', '--dir',
                monitor_dname
            ], **self.PIPES)

            with kill(p):
                # Give monitor time to start watching directory
                time.sleep(0.5)
                with open(monitored_file, 'wb') as f:
                    pass
                # Give monitor time to pickup event
                time.sleep(0.5)

                os.unlink(monitored_file)
                time.sleep(0.5)

                # Create another file
                with open(os.path.join(monitor_dname, 'f2'), 'wb') as f:
                    pass
                time.sleep(0.4)

                p.stdout.close()
                self.assertEqual(0, p.wait(1.0))
                p.stderr.close()
                p.stdin.close()

            with open(checklog, 'rb') as f:
                self.assertGreaterEqual(len(f.readlines()), 3)

    def test_sigusr1_trigger(self):
        """ Monitor checks for updates when it gets SIGUSR1 """
        self.skipTest("Not implemented yet")


if __name__ == '__main__':
    unittest.main()

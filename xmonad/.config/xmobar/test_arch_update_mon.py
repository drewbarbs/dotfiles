import contextlib
import os
import signal
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
                self.assertGreaterEqual(len(f.readlines()), 4)

    def _do_test_dir_change_trigger_action(self, action, preaction=None):
        """ Monitor checks for updates when the monitored directory changes """

        with tempfile.TemporaryDirectory() as monitor_dname,\
               tempfile.TemporaryDirectory() as tmpdir:
            checklog = os.path.join(tmpdir, 'checklog')
            cmd = 'echo something >> {}'.format(checklog)

            if preaction is not None:
                preaction(monitor_dname)

            p = subprocess.Popen([
                TEST_FILE, '--cmd', cmd, '--interval=10000', '--dir',
                monitor_dname
            ], **self.PIPES)

            with kill(p):
                # Give monitor time to start watching directory
                time.sleep(0.5)

                action(monitor_dname)

                time.sleep(0.5)

                p.stdout.close()
                self.assertEqual(0, p.wait(1.0))
                p.stderr.close()
                p.stdin.close()

            # assert that the action caused another check, in addition
            # to the monitor's initial check
            with open(checklog, 'rb') as f:
                self.assertGreaterEqual(len(f.readlines()), 2)

    def test_dir_change_trigger_fedit(self):
        """ Monitor checks for updates when file in monitored directory changes """

        def preaction(monitor_dname):
            # Prestage file
            monitored_file = os.path.join(monitor_dname, 'monitor')
            with open(monitored_file, 'wb') as f:
                f.write(b'a')

        def action(monitor_dname):
            # edit the file
            monitored_file = os.path.join(monitor_dname, 'monitor')
            with open(monitored_file, 'wb') as f:
                f.write(b'b' * 100)

        self._do_test_dir_change_trigger_action(action, preaction)

    def test_dir_change_trigger_funlink(self):
        """ Monitor checks for updates when a file is deleted from monitored dir """

        def preaction(monitor_dname):
            # Prestage file
            monitored_file = os.path.join(monitor_dname, 'monitor')
            with open(monitored_file, 'wb') as f:
                f.write(b'a')

        def action(monitor_dname):
            # remove the file
            monitored_file = os.path.join(monitor_dname, 'monitor')
            os.unlink(monitored_file)

        self._do_test_dir_change_trigger_action(action, preaction)

    def test_dir_change_trigger_create(self):
        """ Monitor checks for updates when a file is added to monitored dir """

        def action(monitor_dname):
            # remove the file
            new_file = os.path.join(monitor_dname, 'newfile')
            with open(new_file, 'wb') as f:
                f.write(b'a')

        self._do_test_dir_change_trigger_action(action)

    def test_dir_change_trigger_pass(self):
        """ Check that the dir change tests fail when no change is made """

        def action(monitor_dname):
            pass

        with self.assertRaises(AssertionError):
            self._do_test_dir_change_trigger_action(action)

    def test_sigusr1_trigger(self):
        """ Monitor checks for updates when it gets SIGUSR1 """

        with tempfile.TemporaryDirectory() as dname:
            # Give a command we can observe result of
            checklog = os.path.join(dname, 'checklog')
            cmd = 'echo something >> {}'.format(checklog)

            p = subprocess.Popen([TEST_FILE, '--cmd', cmd, '--interval=1000'],
                                 **self.PIPES)
            n_signals = 2
            with kill(p):
                # Give app opportunity to install signal handler
                time.sleep(0.5)

                for i in range(2):
                    p.send_signal(signal.SIGUSR1)
                    time.sleep(0.5)

                p.stdout.close()
                self.assertEqual(0, p.wait(1.0))
                p.stdin.close()
                p.stderr.close()

            with open(checklog, 'rb') as f:
                self.assertGreaterEqual(len(f.readlines()), n_signals + 1)


if __name__ == '__main__':
    unittest.main()

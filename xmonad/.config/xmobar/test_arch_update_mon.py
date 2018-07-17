import contextlib
import subprocess
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
    def test_stdout_close(self):
        # Give the monitor a nonsense command to run
        p = subprocess.Popen(
            [TEST_FILE, '--cmd', 'echo something'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        with kill(p):
            with self.assertRaises(subprocess.TimeoutExpired):
                p.wait(0.5)
            p.stdout.close()

            # The process should finish within two seconds
            self.assertEqual(0, p.wait(2.0))

            p.stdin.close()
            p.stderr.close()


if __name__ == '__main__':
    unittest.main()

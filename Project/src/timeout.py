import time


class Timeout:
    def __init__(self, duration):
        self.duration = duration
        self.start_time = time.time()

    def elapsed_time(self):
        return time.time() - self.start_time

    def refresh(self):
        self.start_time = time.time()

    def check_timeout(self):
        return self.elapsed_time() >= self.duration

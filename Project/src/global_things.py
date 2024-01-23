import queue
import threading
from typing import List


node_list: List[{int, queue.Queue}]
thread_list: List[threading.Thread]
list_lock = threading.Lock()

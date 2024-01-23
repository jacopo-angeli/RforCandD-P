import string
from typing import List
import queue
from Message import MessageInterface
from Message import NodeInfo
from NodeState import NodeStateInterface
from Payload import PayloadInterface
from Project import list_lock, node_list, thread_list
from Project import Link
from Project import Timeout


class Node:
    _term: int
    _id: int
    _log: queue.Queue[PayloadInterface]
    _state: NodeStateInterface

    _electionTimeout: Timeout
    _heartbeatTimeout: Timeout

    _messageQueue: queue.Queue[MessageInterface]

    # List[string]: IP degli altri nodi
    # TODO : Check if there is a better way to declare a list of strings
    def __init__(self, id: int, sendMessage: function) -> None:
        # Send NodeInfo to all the network elements
        self._id = id
        self._sendMessage = sendMessage
        self._messageQueue = node_list[self._id]

        while True:
            try:
                message = self._messageQueue.get()
            except queue.Empty:
                print("Message queue empty.")

    def _receiveMessage():
        pass

    def _addNode(nodeInfo: NodeInfo):
        pass

    def _removeNode(node: NodeInfo):
        pass

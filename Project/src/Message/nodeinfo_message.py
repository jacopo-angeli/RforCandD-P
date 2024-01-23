import string
from Message import MessageInterface


class NodeInfo(MessageInterface):
    _id: int
    _address: string  # Rapresentation of IP

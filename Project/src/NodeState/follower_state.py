from node_state_interface import NodeStateInterface


class FollowerState(NodeStateInterface):
    _leader: int

    def __init__(self, leader: int) -> None:
        super().__init__()
        self._leader = leader

    @property
    def leader(self) -> int:
        return self._leader

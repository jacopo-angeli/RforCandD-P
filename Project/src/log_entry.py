from Project.src.Payload.payload_interface import PayloadInterface


class LogEntry:
    _term: int
    _index: int
    _payload: PayloadInterface

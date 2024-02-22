with LogEntry;
with Ada.Containers;
package Message is
    type Message is tagged null record;
    --  AppendEntries RPC
    type AppendEntry is new Message with record
        --  Leader’s term
        Term         : Integer;
        --  So follower can redirect clients
        LeaderId     : Integer;
        --  Index of log entry immediately preceding new ones
        PrevLogIndex : Integer;
        --  Term of prevLogIndex entry
        PrevLogTerm  : Integer;
        --  Log entries to store (empty for heartbeat; may send more than one for efficiency)
        LogEntri     : LogEntry.LogEntry;
        --  Leader’s commitIndex
        LeaderCommit : Integer;
    end record;

    type AppendEntryResponse is new Message with record
        --  CurrentTerm, for leader to update itself
        Term    : Integer;
        --  True if follower contained entry matching prevLogIndex and prevLogTerm
        Success : Boolean;
    end record;

    function Message_Stringify (Msg : in AppendEntry) return String;
    function Message_Stringify (Msg : in AppendEntryResponse) return String;

    --  RequestVote RPC
    type RequestVote is new Message with record
        -- Candidate’s term
        Term         : Integer;
        -- Candidate requesting vote
        CandidateId  : Integer;
        -- Index of candidate’s last log entry (§5.4)
        LastLogIndex : Integer;
        -- Term of candidate’s last log entry (§5.4)
        LastLogTerm  : Integer;
    end record;
    type RequestVoteResponse is new Message with record
        -- currentTerm, for candidate to update itself
        Term        : Integer;
        -- true means candidate received vote
        VoteGranted : Boolean;
    end record;
    function Message_Stringify (Msg : in RequestVote) return String;
    function Message_Stringify (Msg : in RequestVoteResponse) return String;

end Message;

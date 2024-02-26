with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LogEntry;
with Payload;
package Message is
    type Message is abstract tagged null record;
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

    type ClientRequest is new Message with record
        Peyload : Payload.Payload;
    end record;
    type ClientResponse is new Message with record
        Result : Boolean;
        Msg    : Unbounded_String;
    end record;
    function Message_Stringify (Msg : in ClientRequest) return String;
    function Message_Stringify (Msg : in ClientResponse) return String;

end Message;

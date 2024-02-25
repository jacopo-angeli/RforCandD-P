with Ada.Containers.Vectors;
with Ada.Real_Time; use Ada.Real_Time;

with Message;
with Queue;
with LogEntry;
package Node is
  ----------------------------------------------------------------------------  PACKAGES AND RELATED
  -- Pointer to the queue of messages and vector of queue pointers
  type QueueAccess is access all Queue.Queue;
  package QueueVector is new Ada.Containers.Vectors
   (Index_Type => Positive, Element_Type => QueueAccess);
  package LogEntryVector is new Ada.Containers.Vectors
   (Index_Type => Natural, Element_Type => LogEntry.LogEntry,
    "="        => LogEntry."=");

  ---------------------------------------------------------------------------- TYPES

  type NodeType is (LEADER, CANDIDATE, FOLLOWER);

  type NodeState is record

    -----------------------------  Persistent state on all servers:

    --  latest term server has seen (initialized to 0 on first boot,
    --  increases monotonically)
    CurrentTerm : aliased Integer := 0;
    --  candidateId that received vote in current term (or null if
    --  none)
    VotedFor    : aliased Integer := -1;
    --  log entries; each entry contains command for state machine,
    --  and term when entry was received by leader (first index is 1)
    Log         : aliased LogEntryVector.Vector;

    -----------------------------  Volatile state on all servers:

    --  Index of highest log entry known to be committed
    --  (initialized to 0, increases monotonically)
    CommitIndex : aliased Integer;
    --  Index of highest log entry applied to state machine
    --  (initialized to 0, increases monotonically)
    LastApplied : aliased Integer;

    -----------------------------  Volatile state on leaders:

    --  for each server, index of the next log entry to send
    --  to that server (initialized to leader last log index + 1)
    NextIndex  : aliased Integer;
    --  for each server, index of highest log entry known to be
    --  replicated on server (initialized to 0, increases monotonically)
    MatchIndex : aliased Integer;

    -----------------------------  Server simulation params

    CurrentType : aliased NodeType := FOLLOWER;

    HeartbeatTimeoutDuration : aliased Integer;
    ElectionTimeoutDuration  : aliased Integer;
    CandidationTimestamp     : Time;

    AppendedCounter : aliased Integer := 0;
    VotesCounter    : aliased Integer := 0;

    LastPacketTimestamp : aliased Time := Clock;

  end record;

  ----------------------------------------------------------------------------  OBJECTS
  -- QVector instatialization to make it live more than main procedure
  QVector : aliased QueueVector.Vector := QueueVector.Empty_Vector;

  -- Array of the boolean Pause flag to simulate node stopping
  BG : array (1 .. 4) of access Boolean;

  --  Node and Node pointer
  type Node
   (Id : Integer; Net : access QueueVector.Vector; Paused : access Boolean);
  type NodeAccess is access all Node;

  
private

  --  Polymorphic Handle message procedures
  procedure HandleMessage (Self : access NodeState; Msg : Message.AppendEntry);
  procedure HandleMessage
   (Self : access NodeState; Msg : Message.AppendEntryResponse);
  procedure HandleMessage (Self : access NodeState; Msg : Message.RequestVote);
  procedure HandleMessage
   (Self : access NodeState; Msg : Message.RequestVoteResponse);

  --  Broadcast procedure
  procedure Broadcast
   (SelfId : Integer; Net : access QueueVector.Vector;
    Msg    : Message.Message'Class);

  --  Send a message to a specific node using its Id
  procedure Respond
   (Net      : access QueueVector.Vector; Msg : Message.Message'Class;
    Receiver : Integer);

  --  procedure HandleMessage
  --   (Net : access QueueVector.Vector; Id : Integer; Msg : Message.Message'Class;
  --    LastAppendEntryTimestamp : access Time; CurrentTerm : access Integer;
  --    CurrentState : access State; Log : in out LogEntryVector.Vector;
  --    CommitIndex              : access Integer; VotedFor : access Integer;
  --    VotesCounter : access Integer; TimeoutDuration : access Integer);

end Node;

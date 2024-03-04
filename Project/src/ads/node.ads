with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Real_Time; use Ada.Real_Time;

with Message;
with Queue;
with LogEntry; use LogEntry;
with Payload;
package Node is

  ----------------------------------------------------------------------------  PACKAGES AND RELATED

  -- Pointer to the queue of messages and vector of queue pointers
  type QueueAccess is access all Queue.Queue;
  package QueueVector is new Ada.Containers.Vectors
   (Index_Type   => Positive,--
    Element_Type => QueueAccess);
  package IntegerVector is new Ada.Containers.Vectors
   (Index_Type   => Positive,--
    Element_Type => Natural);
  function IntegerVectorInfimum (V : in IntegerVector.Vector) return Integer;
  function EqualityCount
   (V : in IntegerVector.Vector; E : Integer) return Integer;

  ---------------------------------------------------------------------------- TYPES

  type NodeType is (LEADER, CANDIDATE, FOLLOWER);

  type NodeState is record

    -----------------------------  Persistent state on all servers:

    --  latest term server has seen (initialized to 0 on first boot,
    --  increases monotonically)
    CurrentTerm : aliased Integer;
    --  candidateId that received vote in current term (or null if
    --  none)
    VotedFor    : aliased Integer;
    --  log entries; each entry contains command for state machine,
    --  and term when entry was received by leader (first index is 1)
    Log         : aliased LogEntryVector.Vector;
    DB          : aliased Payload.PayloadVector.Vector;

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
    --      The leader maintains a nextIndex for each follower,
    --      which is the index of the next log entry the leader will
    --      send to that follower. When a leader first comes to power,
    --      it initializes all nextIndex values to the index just after the
    --      last one in its log.
    NextIndex  : aliased IntegerVector.Vector;
    --  for each server, index of highest log entry known to be
    --  replicated on server (initialized to 0, increases monotonically)
    MatchIndex : aliased IntegerVector.Vector;

    -----------------------------  Server simulation params

    CurrentType : aliased NodeType;

    -----------------------------------------------------------
    -- Note: Our first try was using Integer type to express --
    -- the timeout durations. Later we found a big error of  --
    -- approsimation in the cast operation from time span to --
    -- integer. Using the correct type of the Real_Time      --
    -- package we found out that all the subtle problem we   --
    -- were experiencing depended from the wrong type usage  --
    -----------------------------------------------------------
    LastPacketTimestamp      : aliased Time;
    HeartbeatTimeoutDuration : aliased Time_Span;
    CandidationTimestamp     : aliased Time;
    ElectionTimeoutDuration  : aliased Time_Span;
    
    LastMoonquakeTimestamp    : aliased Time;

    CurrentLeader : aliased Integer;
    VotesCounter  : aliased Integer;

  end record;

  ----------------------------------------------------------------------------  OBJECTS
  -- QVector instatialization to make it live more than main procedure
  QVector : aliased QueueVector.Vector := QueueVector.Empty_Vector;

  -- Array of the boolean Pause flag to simulate node stopping
  BG : array (1 .. 4) of access Boolean;

  --  Node and Node pointer
  task type Node
   (Id     : Integer; --
    Net    : access QueueVector.Vector; --
    Paused : access Boolean) is
    entry Request
     (Msg : in Message.ClientRequest; Response : out Message.ClientResponse);
  end Node;

  type NodeAccess is access all Node;

private

  -- NodeState init function
  function NodeStateInit (NumberOfNodes : Integer) return NodeState;

  --  Handle message procedures
  procedure HandleMessage
   (Id   : Integer; --
    Net  : access QueueVector.Vector;--
    Self : access NodeState; --
    Msg  : Message.Message'Class);
  procedure HandleAppendEntry
   (Id   : Integer; --
    Net  : access QueueVector.Vector;--
    Self : access NodeState; --
    Msg  : Message.AppendEntry);
  procedure HandleAppendEntryResponse
   (Id   : Integer; --
    Net  : access QueueVector.Vector;--
    Self : access NodeState; --
    Msg  : Message.AppendEntryResponse);
  procedure HandleRequestVote
   (Id   : Integer; --
    Net  : access QueueVector.Vector;--
    Self : access NodeState; --
    Msg  : Message.RequestVote);
  procedure HandleRequestVoteResponse
   (Id   : Integer; --
    Net  : access QueueVector.Vector;--
    Self : access NodeState; --
    Msg  : Message.RequestVoteResponse);
  function HandleClientRequest
   (Id   : Integer; --
    Net  : access QueueVector.Vector;--
    Self : access NodeState; --
    Msg  : Message.ClientRequest) return Message.ClientResponse;

  --  Broadcast procedure
  procedure Broadcast
   (SelfId : Integer;--
    Net    : access QueueVector.Vector;--
    Msg    : Message.Message'Class);

  --  Send a message to a specific node using its Id
  procedure Respond
   (Net      : access QueueVector.Vector;--
    Msg      : Message.Message'Class;--
    Receiver : Integer);

  --  Timeout managment
  procedure TimeoutManagment
   (Id   : Integer;--
    Net  : access QueueVector.Vector;--
    Self : access NodeState);

  -- Printers
  function StateToString (S : in NodeState) return String;

end Node;

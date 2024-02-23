with Ada.Containers.Vectors;
with Ada.Real_Time; use Ada.Real_Time;

with Message;
with Queue;
with LogEntry;
package Node is

  type State is (LEADER, CANDIDATE, FOLLOWER);

  --  Node and access Node
  type Node;
  type NodeAccess is access all Node;

  -- Pointer to the queue of messages and vector of queue pointers
  type QueueAccess is access all Queue.Queue;
  package QueueVector is new Ada.Containers.Vectors
   (Index_Type => Positive, Element_Type => QueueAccess);

  package LogEntryVector is new Ada.Containers.Vectors
   (Index_Type => Natural, Element_Type => LogEntry.LogEntry,
    "="        => LogEntry."=");
  type LogAccess is access all LogEntryVector.Vector;

  -- More on Node task type
  task type Node (Id : Integer; Net : access QueueVector.Vector);

  -- QVector instatialization to make it live more than main procedure
  QVector : aliased QueueVector.Vector := QueueVector.Empty_Vector;
private

  --  Handle message
  procedure HandleMessage
   (Net : access QueueVector.Vector; Id : Integer; Msg : Message.Message'Class;
    LastAppendEntryTimestamp : access Time; CurrentTerm : access Integer;
    CurrentState : access State; Log : in out LogEntryVector.Vector;
    CommitIndex              : access Integer; VotedFor : access Integer;
    VotesCounter : access Integer; TimeoutDuration : access Integer);

  --  Send message to all the other node of the network
  procedure Broadcast
   (Id  : Integer; Net : access QueueVector.Vector;
    Msg : Message.Message'Class);

  --  Send a message to a specific node using its Id
  procedure SendToId
   (Net      : access QueueVector.Vector; Msg : Message.Message'Class;
    Receiver : Integer);

end Node;

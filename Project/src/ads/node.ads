with Ada.Containers.Vectors;
with Ada.Real_Time; use Ada.Real_Time;

with Message;
with Queue;
package Node is

  type State is (LEADER, CANDIDATE, FOLLOWER);

  --  Node and access Node
  type Node;
  type NodeAccess is access all Node;

  type QueueAccess is access all Queue.Queue;
  package QueueVector is new Ada.Containers.Vectors
   (Index_Type => Positive, Element_Type => QueueAccess);

  QVector : aliased QueueVector.Vector := QueueVector.Empty_Vector;

  task type Node (Id : Integer; Net : access QueueVector.Vector);

private

  procedure HandleMessage
   (Net            : access QueueVector.Vector; Id : Integer;
    Msg : Message.Message'Class; Last_Heartbeat : access Time;
    Current_Leader : access Integer; Current_Term : access Integer;
    Current_State  : access State);
  procedure Broadcast
   (Id  : Integer; Net : access QueueVector.Vector;
    Msg : Message.Message'Class);
  procedure SendToLeader
   (Current_Leader : Integer; Net : access QueueVector.Vector;
    Msg            : Message.Message'Class);

end Node;

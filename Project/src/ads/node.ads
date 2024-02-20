with Message;
with Queue;
with Ada.Containers.Vectors;
package Node is
   type State is (LEADER, CANDIDATE, FOLLOWER);

   --  Node and access Node
   type Node;
   type NodeAccess is access all Node;

   type QueueAccess is access all Queue.Queue;
   package QueueVector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => QueueAccess);

   QVector : aliased QueueVector.Vector := QueueVector.Empty_Vector;

   task type Node (Id : Integer; Net : access QueueVector.Vector) is
      entry Send_Message (Msg : in Message.Message'Class);
   end Node;

   procedure Broadcast
     (Id: Integer;Net : access QueueVector.Vector; Msg : Message.Message'Class);
   procedure SendToLeader
     (Current_Leader: Integer; Net : access QueueVector.Vector; Msg : Message.Message'Class);

end Node;

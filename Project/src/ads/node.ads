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

   task type Node (id : Integer; net : access QueueVector.Vector) is
      entry Send_Message (Msg : in Message.Message'Class);
   end Node;

end Node;

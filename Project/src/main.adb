with Ada.Text_IO; use Ada.Text_IO;
with Queue;
with Message;
with Node;

procedure Main is
   Q1 : Node.QueueAccess := new Queue.Queue;
   Q2 : Node.QueueAccess := new Queue.Queue;
   Q3 : Node.QueueAccess := new Queue.Queue;
   Q4 : Node.QueueAccess := new Queue.Queue;

   N1, N2, N3, N4: Node.NodeAccess;
begin
   Node.QueueVector.Append (Node.QVector, Q1);
   Node.QueueVector.Append (Node.QVector, Q2);
   Node.QueueVector.Append (Node.QVector, Q3);
   Node.QueueVector.Append (Node.QVector, Q4);

   N1 := new Node.Node (1, Node.QVector'Access);
   N2 := new Node.Node (2, Node.QVector'Access);
   N3 := new Node.Node (3, Node.QVector'Access);
   N4 := new Node.Node (4, Node.QVector'Access);

end Main;

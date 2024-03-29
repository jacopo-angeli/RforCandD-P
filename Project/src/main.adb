with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Node;                  use Node;
with Payload;
with Queue;
with Message;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;         use Ada.Real_Time;

procedure Main is

   package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);

   Q1 : QueueAccess := new Queue.Queue;
   Q2 : QueueAccess := new Queue.Queue;
   Q3 : QueueAccess := new Queue.Queue;
   Q4 : QueueAccess := new Queue.Queue;

   N1, N2, N3, N4 : NodeAccess;

begin

   QueueVector.Append (QVector, Q1);
   QueueVector.Append (QVector, Q2);
   QueueVector.Append (QVector, Q3);
   QueueVector.Append (QVector, Q4);

   BG (1) := new Boolean'(False);
   BG (2) := new Boolean'(False);
   BG (3) := new Boolean'(False);
   BG (4) := new Boolean'(False);

   N1 := new Node.Node (1, QVector'Access, BG (1));
   N2 := new Node.Node (2, QVector'Access, BG (2));
   N3 := new Node.Node (3, QVector'Access, BG (3));
   N4 := new Node.Node (4, QVector'Access, BG (4));

end Main;

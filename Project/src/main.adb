with Ada.Text_IO; use Ada.Text_IO;
with Queue;
with Message;
with Node;

procedure Main is
   Q1 : Node.QueueAccess := new Queue.Queue;
   Q2 : Node.QueueAccess := new Queue.Queue;
   Q3 : Node.QueueAccess := new Queue.Queue;
   Q4 : Node.QueueAccess := new Queue.Queue;

   N1, N2, N3, N4 : Node.NodeAccess;

   
begin
   Node.QueueVector.Append (Node.QVector, Q1);
   Node.QueueVector.Append (Node.QVector, Q2);
   Node.QueueVector.Append (Node.QVector, Q3);
   Node.QueueVector.Append (Node.QVector, Q4);
   
   Node.BG(1) := new Boolean'(False);
   Node.BG(2) := new Boolean'(False);
   Node.BG(3) := new Boolean'(False);
   Node.BG(4) := new Boolean'(False);

   N1 := new Node.Node (1, Node.QVector'Access, Node.BG (1));
   N2 := new Node.Node (2, Node.QVector'Access, Node.BG (2));
   N3 := new Node.Node (3, Node.QVector'Access, Node.BG (3));
   N4 := new Node.Node (4, Node.QVector'Access, Node.BG (4));


   declare
      A, N : String (1 .. 1);
   begin
      loop
         Put_Line ("1 : Resume, 2 : Pause");
         Put ("Choice: ");
         Get(A);
         Put_Line ("Node number (1...4) : ");
         Put ("Choice: ");
         Get (N);
         if A = "1" then
            Node.BG (Integer'Value (N)).all := False;
         else
            Node.BG (Integer'Value (N)).all := True;
         end if;
      end loop;
   end;

end Main;

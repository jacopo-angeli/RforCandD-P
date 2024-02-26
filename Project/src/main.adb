with Ada.Text_IO; use Ada.Text_IO;
with Queue;
with Message;
with Node;        use Node;

procedure Main is
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
   delay 0.1;
   N2 := new Node.Node (2, QVector'Access, BG (2));
   delay 0.1;
   N3 := new Node.Node (3, QVector'Access, BG (3));
   delay 0.1;
   N4 := new Node.Node (4, QVector'Access, BG (4));

   declare
      A, N, V : String (1 .. 1);
   begin
      loop
         Put_Line ("1 : Node managment, 3: Client operation");
         Put ("Choice: ");
         Get (A);

         if A = "1" or A = "2" then
            Put ("0 to Resume, 1 to Pause : ");
            Get (A);
            Put ("Node number (1...4) : ");
            Get (N);

            BG (Integer'Value (N)).all := Integer'Value (A) /= 0;
         else
            Put
              ("0 to Transfer money, 1 to Open Account, 2 to withdraw, 3 to Deposit : ");
            Get (A);
            case Integer'Value (A) is
               when 0 =>
                  --  MoneyTranfer
                  Put_Line("MoneyTranfer");
               when 1 =>
                  --  Open Account
                  Put_Line("Open Account");
               when 2 =>
                  --  Withdraw
                  Put_Line("Withdraw");
               when 3 =>
                  --  Deposit
                  Put_Line("Deposit");
               when others =>
                  --  Invalid operation
                  Put_Line("Invalid operation");
            end case;
         end if;
      end loop;
   end;

end Main;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Node;                  use Node;
with Payload;
with Queue;
with Message;

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
                  declare

                     P      : Payload.Payload;
                     FA, TA : Unbounded_String;
                     Q      : Float;

                  begin

                     Put ("From account: ");
                     Get (FA);
                     Put ("To account: ");
                     Get (TA);
                     Put ("Quantity: ");
                     Get (Q);

                     P :=
                       Payload.Payload'
                         (Sort         => Payload.MONEYTRANSFER,--
                          From_Account => FA,--
                          To_Account   => TA,--
                          Quantity     => Float'Value (Q),--
                          New_Account  => "",--
                          Owner        => "");

                  end;
               when 1 =>
                  --  Open Account
                  declare

                     P : Payload.Payload;
                     O : Unbounded_String;

                  begin

                     Put ("Owner name: ");
                     Get (O);

                     P :=
                       Payload.Payload'
                         (Sort         => Payload.ACCOUNTOPEN,--
                          From_Account => "",--
                          To_Account   => "",--
                          Quantity     => 0.0,--
                          New_Account  => "",--
                          Owner        => O);

                  end;
               when 2 =>
                  --  Withdraw
                  declare

                     P  : Payload.Payload;
                     FA : Unbounded_String;
                     Q  : Float;

                  begin

                     Put ("From account: ");
                     Get (FA);
                     Put ("Quantity: ");
                     Get (Q);

                     P :=
                       Payload.Payload'
                         (Sort         => Payload.WITHDRAWAL,--
                          From_Account => FA,--
                          To_Account   => "",--
                          Quantity     => Float'Value (Q),--
                          New_Account  => "",--
                          Owner        => "");

                  end;
               when 3 =>
                  --  Deposit
                  declare

                     P  : Payload.Payload;
                     TA : Unbounded_String;
                     Q  : Float;

                  begin

                     Put ("To account: ");
                     Get (TA);
                     Put ("Quantity: ");
                     Get (Q);

                     P :=
                       Payload.Payload'
                         (Sort         => Payload.DEPOSIT,--
                          From_Account => "",--
                          To_Account   => TA,--
                          Quantity     => Float'Value(Q),--
                          New_Account  => "",--
                          Owner        => "");

                  end;
               when others =>
                  --  Invalid operation
                  Put_Line ("Invalid operation");
            end case;
         end if;
      end loop;
   end;

end Main;

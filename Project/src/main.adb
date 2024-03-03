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

   --  declare
   --     Action      : String (1 .. 1);
   --     N           : String (1 .. 1);
   --     Generated   : Integer := 0;
   --     LowerBound  : Integer := 0;
   --     UpperBound  : Integer := 10;
   --     Probability : Integer := 5;
   --     Gen         : Integer_Random.Generator;
   --  begin

   --     --  Generated :=
   --     --    Integer_Random.Random (Gen) mod UpperBound +
   --     --    LowerBound; --Genero un intero random nell intervallo [LowerBound, UpperBound]
   --     --  -- for N in 0..3 loop

   --     --  if
   --     --    (Generated >= Probability / 2 and
   --     --     Generated <= (Probability + (Probability / 2)))
   --     --  then
   --     --     BG (Integer'Value (N)).all :=
   --     --       True; --for poisson rules ragionamento eremitico durato circa 30 secondi
   --     --  else
   --     --     BG (Integer'Value (N)).all := False;
   --     --  end if;
   --     -- end loop;

   --     --loop
   --     --Put ("1 - Node managment, 2 - Send request :: ");
   --     --Get (Action);
   --     --case Integer'Value (Action) is

   --     --when 1 =>
   --     --declare
   --     -- N : String (1 .. 1);
   --     --begin
   --     -- Put ("1 - Stop, 2 - Resume :: ");
   --     --Get (Action);
   --     --Put ("Node number :: ");
   --     --Get (N);
   --     --case Integer'Value (Action) is
   --     --   when 1 =>
   --     --      BG (Integer'Value (N)).all := True;

   --     --   when 2 =>
   --     --      BG (Integer'Value (N)).all := False;

   --     -- when others =>
   --     --  Put_Line ("Invalid input.");

   --     --end case;
   --     --end;

   --     --when 2 =>
   --     -- declare

   --     --  ReqType : String (1 .. 1);

   --     --Req : Message.ClientRequest;
   --     --Res : Message.ClientResponse;
   --     --PP  : Payload.Payload;

   --     --begin
   --     -- Put ("Request type :: ");
   --     -- Get (ReqType);
   --     --case Integer'Value (ReqType) is
   --     -- when others =>
   --     --  TODO : Manage of different type of request
   --     --  PP  := Payload.EmptyPayload;
   --     --Req := Message.ClientRequest'(Peyload => PP);
   --     --  N1.Request (Req, Res);
   --     --  if Res.Result = False then
   --     --     N2.Request (Req, Res);
   --     --  end if;
   --     --  if Res.Result = False then
   --     --     N3.Request (Req, Res);
   --     --  end if;
   --     --  if Res.Result = False then
   --     --     N4.Request (Req, Res);
   --     --  end if;
   --     --end case;
   --     --Put_Line
   --     --("Response {Result : " & Boolean'Image (Res.Result) &
   --     --  ", body : " & To_String (Res.Msg) & "}");
   --     -- end;
   --     --when others =>
   --     -- Put_Line ("Choice not valid.");
   --     --end case;
   --     --end loop;
   --  end;

end Main;

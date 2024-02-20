with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Message;       use Message;
with LogEntry;

package body Node is

   task body Node is

      Current_Term : aliased Integer := 0;

      Current_State : aliased State := FOLLOWER;

      package LogEntry_Vector is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => LogEntry.LogEntry,
         "="        => LogEntry."=");
      Log : LogEntry_Vector.Vector;

      Current_Leader : aliased Integer := -1;

      Appended_Counter : Integer := 0;
      Votes_Counter    : Integer := 0;

      Append_Happening : Boolean;
      Vote_Happening   : Boolean;

      Last_Heartbeat : aliased Time := Clock;

      Election_Timeout_Duration : Integer;

      Heartbeat_Timeout_Duration : Integer;

      package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
      Gen : Integer_Random.Generator;

   begin

      Integer_Random.Reset (Gen);
      Election_Timeout_Duration := Integer_Random.Random (Gen) mod 151 + 1_500;
      Heartbeat_Timeout_Duration := Integer_Random.Random (Gen) mod 101 + 500;
      if Id = 1 then
         Broadcast (Id, Net, Message.Heartbeat'(Sender_Id => Id, Term => Id));
      end if;

      loop
         Put_Line
           ("Node(" & Integer'Image (Id) & " ) running with leader " &
            Integer'Image (Current_Leader) & ";");
         declare
            Start_Time, End_Time : Time;
            Milliseconds         : Integer := 0;
         begin
            if not Queue.Is_Empty (net.all (id).all) then
               Start_Time := Clock;

               HandleMessage
                 (Net, Id, Queue.Dequeue (net.all (Id).all),
                  Last_Heartbeat'Access, Current_Leader'Access,
                  Current_Term'Access, Current_State'Access);

               End_Time := Clock;

               Milliseconds :=
                 Integer (To_Duration (End_Time - Start_Time)) * 1_000;

               -- Managment Last_Heartbeat Usare Millisecond

            end if;
         end;
         delay 1.0;
      end loop;
   end Node;

   procedure Broadcast
     (Id  : Integer; Net : access QueueVector.Vector;
      Msg : Message.Message'Class)
   is
   begin
      for I in Net.all.First_Index .. Net.all.Last_Index loop
         if I /= Id then
            Queue.Enqueue (net.all (I).all, Msg);
         end if;
      end loop;
   end Broadcast;

   procedure SendToLeader
     (Current_Leader : Integer; Net : access QueueVector.Vector;
      Msg            : Message.Message'Class)
   is
   begin
      Put_Line ("Boiadioasidoki");
   end SendToLeader;

   procedure HandleMessage
     (Net            : access QueueVector.Vector; Id : Integer;
      Msg            : Message.Message'Class; Last_Heartbeat : access Time;
      Current_Leader : access Integer; Current_Term : access Integer;
      Current_State  : access State)
   is
   begin
      case Current_State.all is
         when FOLLOWER =>
            if Msg in Heartbeat'Class then
               Last_Heartbeat.all := Clock;
               Current_Leader.all := Msg.Sender_Id;
               Broadcast (Id, Net, Commit'(Sender_Id => Id, Term => 12_837));
            else
               Put_Line (Message_Stringify (Msg));
            end if;
         when others =>
            null;
      end case;
   end HandleMessage;

end Node;

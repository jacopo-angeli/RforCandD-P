with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;

with Message;
with Logger;
with LogEntry;

package body Node is
   use Ada.Text_IO;
   use Ada.Real_Time;
   use Ada.Strings.Fixed;
   use Message;
   use Logger;

   task body Node is

      Current_Term : aliased Integer := 0;

      Current_State : aliased State := FOLLOWER;

      Log : aliased LogEntryVector.Vector;
      Log_Pointer : LogAccess:=Log'Access;

      Current_Leader : aliased Integer := -1;
      Appended_Counter : Integer := 0;
      Votes_Counter    : Integer := 0;

      Append_Happening : Boolean;
      Vote_Happening   : Boolean;

      Last_Heartbeat : aliased Time := Clock;
      CUrrent_Time   : Time         := Clock;

      Election_Timeout_Duration : Integer;

      Heartbeat_Timeout_Duration       : Integer;
      Milliseconds_From_Last_Heartbeat : Duration;

      package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
      Gen : Integer_Random.Generator;

   begin
      Integer_Random.Reset (Gen);
      Election_Timeout_Duration := Integer_Random.Random (Gen) mod 151 + 1_500;
      Heartbeat_Timeout_Duration := Integer_Random.Random (Gen) mod 101 + 500;
      Milliseconds_From_Last_Heartbeat :=
        To_Duration (CUrrent_Time - Last_Heartbeat) * 1_000;
      if Id = 1 then
         Broadcast (Id, Net, Message.Heartbeat'(Sender_Id => Id, Term => Current_Term, Log_length => Log.Length));
         Current_State:=LEADER;
      end if;

      loop
         Log_Row
           (File_Name => "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left),
            Row       =>
              Table_Row'
                (Id             => Id, Current_Leader => Current_Leader,
                 Current_Term => Current_Term, Current_State => Current_State,
                 Vector_Entries => Log));
         declare
            Start_Time, End_Time : Time;
            Milliseconds         : Integer := 0;
         begin
            --Managment Last_Heartbeat using milliseconds
            if (Current_State = LEADER) then
               if
                 (Integer (Milliseconds_From_Last_Heartbeat) >
                  Heartbeat_Timeout_Duration + Milliseconds)
               then
               -- Heartbeat expired
               -- Send heartbeat to all the nodes
               -- Make Last_Heartbeat = 0
               Broadcast (Id, Net, Message.Heartbeat'(Sender_Id => Id, Term => Current_Term, Log_length=> Log.Length));
               Last_Heartbeat := Clock;
            end if;
         end if;
         --Follower election timeout managment 
         if(Current_State = FOLLOWER) then 
            if(Integer (Milliseconds_From_Last_Heartbeat) >
               Election_Timeout_Duration + Milliseconds)
               then
               -- Election timeout expired
               -- Set state to CANDIDATE
               -- Update votes counter
               -- Update the term variable
               -- Send candidation to all the nodes
               -- Make Last_Heartbeat = 0
               Current_State:=CANDIDATE;
               Current_Term:=Current_Term+1;
               Votes_Counter:=Votes_Counter+1;
               Broadcast (Id, Net, Message.Candidated'(Sender_Id => Id, Term => Current_Term, Log_length=> Log.Length));
               Last_Heartbeat := Clock;
            end if;
         end if;

            if not Queue.Is_Empty (net.all (id).all) then
               Start_Time := Clock;

               HandleMessage
                 (Net, Id, Queue.Dequeue (net.all (Id).all),
                  Last_Heartbeat'Access, Current_Leader'Access,
                  Current_Term'Access, Current_State'Access, Log_Pointer);
               End_Time := Clock;
               Milliseconds :=
                 Integer (To_Duration (End_Time - Start_Time)) * 1_000;
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

   procedure SendToId
      (Net : access QueueVector.Vector;
      Msg : Message.Message'Class;
      Reciever :  Integer) is
   begin
            Queue.Enqueue (net.all(Reciever).all, Msg);
     
   end SendToId;
   procedure HandleMessage
     (Net            : access QueueVector.Vector; Id : Integer;
      Msg            : Message.Message'Class; Last_Heartbeat : access Time;
      Current_Leader : access Integer; Current_Term : access Integer;
      Current_State  : access State;
      Log            : LogAccess)

   is
   Log_Length : aliased Ada.Containers.Count_Type := Log.all.Length;
   begin
      case Current_State.all is
         when FOLLOWER =>
            if Msg in Heartbeat'Class then
               Last_Heartbeat.all := Clock;
               Current_Leader.all := Msg.Sender_Id;
               --Broadcast (Id, Net, Commit'(Sender_Id => Id, Term => 12_837, Log_length => Log_Length));

            elsif Msg in Candidated'Class then
               --Election handling
               if Log_Length<Msg.Log_length then 
                  --node has to vote for that candidate
                  SendToId(Net, Message.Vote'(Current_Term.all, Id, Log_Length), Msg.Sender_Id);                  
               end if;

            elsif Msg in Commit'Class then
               -- the node has to commit that Log entry (i.e change the entry state in COMMITED)
               
               for I in Log.all.First_Index .. Log.all.Last_Index loop
                  if Log.all(I).State=APPENDEDD then 
                     Log.all(I).State:=COMMITED;
                  end if;
               end loop;

            elsif Msg in AppendEntry'Class then 
               -- check the validity of the append operation on the Log 
               null;
            end if;
         when CANDIDATE =>
            null;
         When LEADER =>
            null;
      end case;
   end HandleMessage;

end Node;

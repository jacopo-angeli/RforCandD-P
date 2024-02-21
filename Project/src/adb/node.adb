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
   use LogEntry;

   task body Node is

      Current_Term : aliased Integer := 0;

      Current_State : aliased State := FOLLOWER;

      Log : aliased LogEntryVector.Vector;

      Current_Leader   : aliased Integer := -1;
      Appended_Counter : Integer         := 0;
      Votes_Counter    : Integer := 0;

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
         Broadcast
           (Id, Net,
            Message.Heartbeat'
              (Sender_Id  => Id, Term => Current_Term,
               Log_length => Integer (Log.Length)));
         Current_State := LEADER;
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
                  Broadcast
                    (Id, Net,
                     Message.Heartbeat'
                       (Sender_Id  => Id, Term => Current_Term,
                        Log_length => Integer (Log.Length)));
                  Last_Heartbeat := Clock;
               end if;
            end if;
            --Follower election timeout managment
            if (Current_State = FOLLOWER) then
               if
                 (Integer (Milliseconds_From_Last_Heartbeat) >
                  Election_Timeout_Duration + Milliseconds)
               then
                  -- Election timeout expired
                  -- Set state to CANDIDATE
                  -- Update votes counter
                  -- Update the term variable
                  -- Send candidation to all the nodes
                  -- Make Last_Heartbeat = 0
                  Current_State := CANDIDATE;
                  Current_Term  := Current_Term + 1;
                  Votes_Counter := Votes_Counter + 1;
                  Broadcast
                    (Id, Net,
                     Message.Candidated'
                       (Sender_Id  => Id, Term => Current_Term,
                        Log_length => Integer (Log.Length)));
                  Last_Heartbeat := Clock;
               end if;
            end if;

            if not Queue.Is_Empty (net.all (id).all) then
               Start_Time := Clock;
               HandleMessage
                 (Net, Id, Queue.Dequeue (net.all (Id).all),
                  Last_Heartbeat'Access, Current_Leader'Access,
                  Current_Term'Access, Current_State'Access, Log, Votes_Counter);
               End_Time     := Clock;
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
     (Net      : access QueueVector.Vector; Msg : Message.Message'Class;
      Reciever : Integer)
   is
   begin
      Queue.Enqueue (net.all (Reciever).all, Msg);

   end SendToId;

   procedure HandleMessage
     (Net            : access QueueVector.Vector; Id : Integer;
      Msg            : Message.Message'Class; Last_Heartbeat : access Time;
      Current_Leader : access Integer; Current_Term : access Integer;
      Current_State  : access State; Log : LogEntryVector.Vector;
      Votes_Counter :   in out Integer)
   is
      Log_Length : Integer := Integer (Log.Length);

      procedure DeleteInconsistentEntries
        (Log : LogEntryVector.Vector; Index : Integer)
      is
      begin
         Log.Delete (Index, Log.Length - Index + 1);
      end DeleteInconsistentEntries;

      Log_Length : Integer := Integer (Log.Length);   
      Nodes_number : Integer:= Integer(Net.all.Length);
   begin
      case Current_State.all is
         when FOLLOWER =>
            if Msg in Heartbeat'Class then
               Last_Heartbeat.all := Clock;
               Current_Leader.all := Msg.Sender_Id;

            elsif Msg in Candidated'Class then
               -- Election handling
               if Log_Length < Msg.Log_length then
                  -- Node has to vote for that candidate
                  SendToId
                    (Net, Message.Vote'(Current_Term.all, Id, Log_Length),
                     Msg.Sender_Id);
               end if;

            elsif Msg in Commit'Class then
               -- the node has to commit that Log entry (i.e change the entry state in COMMITED)

               for I in Log.First_Index .. Log.Last_Index loop
                  if Log (I).State = LogEntry.APPENDED then
                     declare
                        Element : LogEntry.LogEntry := Log (I);
                     begin
                        Set_State (Element, LogEntry.COMMITTED);
                     end;
                  end if;
               end loop;

            elsif Msg in AppendEntry'Class then
               --  Identify Inconsistencies: It first checks for any discrepancies between its log and the incoming entries,
               --   specifically looking for entries at the same index but with different terms.
               --  Resolve Conflicts: If inconsistencies are found, the node deletes the conflicting entry and all
               --   subsequent entries in its log to align with the leader's log.
               --  Append New Entries: The node then appends any new entries from the leader that it doesn't already have,
               --   ensuring its log matches the leader's.
               --  Update Commit Index: If the leader's commit index is higher than the node's, the node updates its commit
               --   index to reflect the latest committed entry, capped by the last new entry appended from the current RPC.
               --  Respond to Leader: Finally, the node sends a response back to the leader. This response indicates success
               --   if the entries were appended without issue or details of any conflict if inconsistencies were detected
               --   and resolved.
               declare
                  Incoming_Entry       : LogEntry.LogEntry :=
                    AppendEntry (Msg).LogEntri.Term;
                  Incoming_Entry_Term  : Integer           :=
                    AppendEntry (Msg).LogEntri.Term;
                  Incoming_Entry_Index : Integer           :=
                    AppendEntry (Msg).LogEntri.Index;

                  Last_Entry       : LogEntry.LogEntry;
                  Last_Entry_Term  : Integer := 0;
                  Last_Entry_Index : Integer := 0;
               begin
                  --  If the Log is not empty initialization of parameters
                  if not Log.Is_Empty then
                     Last_Entry       := Log (Log.Last_Index);
                     Last_Entry_Term  := Last_Entry.Term;
                     Last_Entry_Index := Last_Entry.Index;
                  end if;

                  if Incoming_Entry_Index = Last_Entry_Index + 1 and
                    Incoming_Entry_Term = Last_Entry_Term
                  then
                     --  Valid Entry
                     LogEntryVector.Append (Log, Incoming_Entry);
                     SendToLeader
                       (Current_Leader => Current_Leader.all, Net => Net,
                        Msg            =>
                          Appended'(Id, Current_Term, Integer (Log.Length)));

                  elsif Incoming_Entry_Index <= Last_Entry_Index then
                     --  Local index beside incoming index
                     DeleteInconsistentEntries (Log => Log, Index => Incoming_Entry_Index);
                     LogEntryVector.Append (Container => Log, New_Item => Incoming_Entry);

                  else
                     --  Missing some entries
                     SendToLeader
                       (Current_Leader => Current_Leader.all, Net => Net,
                        Msg            =>
                          LogOutdated'
                            (Sender_Id  => Id, Term => Current_Term,
                             Log_length => Integer (Log.Length),
                             LogEntri   => Last_Entry));
                  end if;

               end;

            end if;
         when CANDIDATE =>
            for I in Net.all.First_Index .. Net.all.Last_Index loop
               if Msg in Vote'Class then 
                  --Votation handling
                  Votes_Counter:=Votes_Counter+1;
               end if;
            end loop;
            if(Votes_Counter > (Nodes_number/2)) then
               --Node becomes the leader
               Current_State.all:=LEADER;
            end if;
         when LEADER =>
            null;
      end case;
   end HandleMessage;

end Node;

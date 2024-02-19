with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Message;
with LogEntry;

package body Node is

   task body Node is
      --  NODE ATTRIBUTES

      --  Each term begins with an election, where one of the nodes in the cluster is elected as the leader for that term. If the election is unsuccessful (e.g., due to a split vote), a new term is started, and a new election process takes place.
      --  Within a given term, each node may vote for only one candidate ensuring that there is at most one leader in a term.
      --  The term numbers are also used in the log entries. Each log entry contains the term number when the entry was received by the leader. This helps with maintaining a consistent and ordered log across all the nodes.
      --  A node won't accept a log from a leader of a previous term, which prevents inconsistencies and ensures that committed entries are not overwritten.
      --  Each term is identified by a unique, consecutive integer.
      Current_Term : Integer := 0;

      --  A node can be in three states : LEADER, CANDIDATE and FOLLOWER
      --  A node is created with the FOLLOWER state
      Current_State : State := FOLLOWER;
       
      --  Log
      package LogEntry_Vector is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => LogEntry.LogEntry);   
      Log : LogEntry_Vector.Vector;

      --  Leader
      Current_Leader: Integer;

      --  COUNTERS
      Appended_Counter : Integer; 
      Votes_Counter : Integer;
      
      --  TIMEOUTS
      Last_Heartbeat : Time;

      --  The election timeout is the amount of time a follower waits until becoming a candidate
      --  The election timeout is randomized to be between 150ms and 300ms.
      Election_Timeout_Duration : Integer; -- 150 ms

      --  The heartbeat timeout is the fixed interval for leaders to send regular heartbeats to followers, ensuring ongoing leadership presence.
      --  It's typically set between 50ms to 150ms to maintain constant communication and prevent unnecessary elections.
      Heartbeat_Timeout_Duration : Integer;  -- 300ms

      --  RANDOM NUMBER GENERATOR
      package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
      Gen : Integer_Random.Generator;
   begin
      --  INIT
      Integer_Random.Reset (Gen);
      Election_Timeout_Duration  := Integer_Random.Random (Gen) mod 151 + 150;
      Heartbeat_Timeout_Duration := Integer_Random.Random (Gen) mod 101 + 50;

      -- SERVER LOOP
      loop
         select
            --  Strucutre of the send_message procedure
            -- FOLLOWER
            --  Heartbeat
            --  AppendEntry
            --  Commit
            --  Candidated
            -- CANDIDATE
            --  Vote
            -- LEADER
            --  Appendend
            --  Commited
            --  ClientOperation
            accept Send_Message (Msg : in Message.Message'Class) do
               case Current_State is
                  when FOLLOWER =>
                     if Msg in Heartbeat'Class then
                        --  Reset last heartbeat receive time
                        if (Current_Leader == Msg.Sender_Id & Current_Term <= Msg.Term) then
                           Last_Heartbeat := Clock;
                           end if;
                     elsif Msg in AppendEntry'Class then
                        -- if the entry has index equal to the lenght of Log.Length + 1 
                        --  Add it + return Appended message
                        -- else
                        --  return LogOutdated message
                        declare
                           -- Declaration 
                           AppendEntryMessage : Message.AppendEntry;
                        begin 
                           -- Init
                           AppendEntryMessage:= Message.AppendEntry(Msg);

                           -- Exec
                           if(AppendEntryMessage.LogEntri.Index = (Log.Last_Element.Index + 1)) then
                              Log.Append(AppendEntryMessage.LogEntri);
                              net.all(Current_Leader).Send_Message(Message.Appended'(Sender_Id => id, Term => Current_Term));
                           else
                              net.all(Current_Leader).Send_Message(Message.LogOutdated'(Sender_Id => id, LogEntri => AppendEntryMessage.LogEntri)); 
                           end if;
                        end;
                     elsif Msg in Commit'Class then
                        -- Set last log entry as COMMITED
                        Log.Last_Element.state := LogEntry.LogEntryState.COMMITED;
                        -- Send COMMITED message to LEADER
                        net.all(Current_Leader).Send_Message(Message.Commited'(Sender_Id => id, Term => Current_Term));
                     elsif Msg in Candidated'Class then
                        -- Send vote to Sender_Id 
                        net.all(Msg.Sender_Id).Send_Message(Message.Vote'(Sender_Id => id));
                     else
                        null; -- Or handle unsupported message types
                     end if;

                  when CANDIDATE =>
                     if Msg in Vote'Class then
                        null;
                     else
                        null; -- Or handle unsupported message types
                     end if;

                  when LEADER =>
                     if Msg in Appended'Class then
                        --  Needs a list of received appended message

                     -- Handle Appended
                     elsif Msg in Committed'Class then
                     -- Handle Committed
                     elsif Msg in ClientOperation'Class then
                     -- Handle ClientOperation
                     else
                        null; -- Or handle unsupported message types
                     end if;
               end case;
               null;
            end Send_Message;
         or
            delay 1.0;
         end select;
         
         --  TODO : Check when this has to happen, now or before accepting messages ?
        
            declare
               --  DECLARATION
               Current_Time                     : Time;
               Milliseconds_From_Last_Heartbeat : Duration;
            begin
               --  INIT
               Current_Time                     := Clock;
               Milliseconds_From_Last_Heartbeat :=
                 To_Duration (Current_Time - Last_Heartbeat) * 1_000;

               --  RUN
               if Current_State = LEADER then
               --  LEADER
               --  Heartbeat timeout managment
                  if(Milliseconds_From_Last_Heartbeat > Integer'Duration(Heartbeat_Timeout_Duration)) then
                     -- Heartbeat expired
                     -- Send heartbeat to all the node
                     -- Make leader counter == 0
                     for i in net.all'range loop
                        net.all(i).Send_Message(Msg => Message.Heartbeat'(Term => Term, Sender_Id => id));
                     end loop;
                     Milliseconds_From_Last_Heartbeat:= Duration(0,0,0,0);
                  end if;
               elsif Current_State = FOLLOWER then
               -- FOLLOWER
               --  Election timeout managment
                  if(Milliseconds_From_Last_Heartbeat > Integer'Duration(Election_Timeout_Duration)) then
                  --    If expired
                  --    State = Candidate
                  --    Votes for itself
                  --    Asks for votes
                     Current_State:=CANDIDATE;


                  end if;
               else
               --  TODO and TBD
               end if;
            end;

         

      end loop;
   end Node;

end Node;

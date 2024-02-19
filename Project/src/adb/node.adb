with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Message;

package body Node is

   task body Node is
      --  NODE ATTRIBUTES

      --  Each term begins with an election, where one of the nodes in the cluster is elected as the leader for that term. If the election is unsuccessful (e.g., due to a split vote), a new term is started, and a new election process takes place.
      --  Within a given term, each node may vote for only one candidate ensuring that there is at most one leader in a term.
      --  The term numbers are also used in the log entries. Each log entry contains the term number when the entry was received by the leader. This helps with maintaining a consistent and ordered log across all the nodes.
      --  A node won't accept a log from a leader of a previous term, which prevents inconsistencies and ensures that committed entries are not overwritten.
      --  Each term is identified by a unique, consecutive integer.
      Term : Integer := 0;

      --  A node can be in three states : LEADER, CANDIDATE and FOLLOWER
      --  A node is created with the FOLLOWER state
      Current_State : State := FOLLOWER;

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
                     -- Handle Heartbeat
                     elsif Msg in AppendEntry'Class then
                     -- Handle AppendEntry
                     elsif Msg in Commit'Class then
                     -- Handle Commit
                     elsif Msg in Candidated'Class then
                     -- Handle Candidated
                     else
                        null; -- Or handle unsupported message types
                     end if;

                  when CANDIDATE =>
                     if Msg in Vote'Class then
                     -- Handle Vote
                     else
                        null; -- Or handle unsupported message types
                     end if;

                  when LEADER =>
                     if Msg in Appended'Class then
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
               --   If expired
               --    Send heartbeat to all the node
               elsif Current_State = FOLLOWER then
               -- FOLLOWER
               --  Election timeout managment
               --    If expired
               --     State = Candidate
               --     Asks for votes
               else
               --  TODO and TBD
               end if;
            end;

         

      end loop;
   end Node;

end Node;

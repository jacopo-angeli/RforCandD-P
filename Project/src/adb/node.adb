with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Message;       use Message;
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
        (Index_Type => Natural, Element_Type => LogEntry.LogEntry,
         "="        => LogEntry."=");
      Log : LogEntry_Vector.Vector;

      --  Leader
      Current_Leader : Integer := -1;

      --  COUNTERS
      Appended_Counter : Integer := 0;
      Votes_Counter    : Integer := 0;

      --  FLAG
      Append_Happening : Boolean;
      Vote_Happening   : Boolean;

      --  TIMEOUTS
      Last_Heartbeat : Time := Clock;

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
      Election_Timeout_Duration := Integer_Random.Random (Gen) mod 151 + 1_500;
      Heartbeat_Timeout_Duration := Integer_Random.Random (Gen) mod 101 + 500;

      Broadcast (Id, Net, Message.Heartbeat'(Sender_Id => Id, Term => Id));
      delay 2.0;
      loop
         --  Managment dei messaggi nella queue
         --  Se non ci sono messaggi entro timeout
         --  Inserisco messaggi su altri nodi della rete
         Put_Line
           ("Node(" & Integer'Image (id) & " ) running with" &
            Ada.Containers.Count_Type'Image (Queue.Length (net.all (id).all)) &
            " messages;");
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
            Queue.Enqueue
              (net.all (I).all,
               Message.Heartbeat'(Sender_Id => id, Term => id));
         end if;
      end loop;
   end Broadcast;

   procedure SendToLeader
     (Current_Leader  : Integer; Net : access QueueVector.Vector;
      Msg : Message.Message'Class)
   is
   begin
      Put_Line ("Boiadioasidoki");
   end SendToLeader;

end Node;

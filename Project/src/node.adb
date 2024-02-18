with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body Node is

   task body Node is
      --  ATTRIBUTES
      term                       : Integer := 0;
      currentState               : State   := FOLLOWER;
      --  TIMER
      Election_Timeout           : Time    := Clock;
      Election_Timeout_Duration  : Integer := 150; -- 150 ms
      Last_Heartbeat_Recevied    : Time    := Clock;
      Heartbeat_Timeout_Duration : Integer := 300;  -- 300ms
   begin
      -- Loop principale del Server
      loop
         Put_Line ("Loop");
         select
            accept SendMessage (Msg : in String) do
               Put_Line
                 ("Node" & Integer'Image (id) & " received message: " & Msg);
            end SendMessage;
         or
            delay 1.0;
         end select;

         declare
            --  Milliseconds from last heartbeat
            Current_Time                     : Time;
            Milliseconds_From_Last_Heartbeat : Duration;
            

         begin
            --  Intialization
            Current_Time                     := Clock;
            Milliseconds_From_Last_Heartbeat :=
              To_Duration (Current_Time - Last_Heartbeat_Recevied) * 1_000;
            
            
            
            --  Put_Line
            --    ("Milliseconds passed from last heartBeat: " &
            --     Duration'Image ((Milliseconds_From_Last_Heartbeat)));

            if (Milliseconds_From_Last_Heartbeat > 300.0) then
            -- Heartbeat missing
            -- State to Candidate
            -- Request votes to other nodes

            end if;
         end;

      end loop;
   end Node;

end Node;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

package body Node is

   task body Node is
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
            delay 0.1;
         end select;
         if id = 1 then
            net.all (2).SendMessage ("Message from Node 1");
         end if;
      end loop;
   end Node;

end Node;

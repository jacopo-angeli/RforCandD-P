package body Payload is
    function EmptyPayload return Payload is
    begin
        return Payload'
              (EMPTY, To_Unbounded_String (""), To_Unbounded_String (""), 0.0,
               To_Unbounded_String (""), To_Unbounded_String (""));
    end EmptyPayload;
end Payload;

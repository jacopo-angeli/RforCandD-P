package body Payload is
    function EmptyPayload return Payload is
    begin
        return
           Payload'
              (Frequency => 0.0, Amplitude => 0.0, Duration => 0,
               Magnitudo => 0, Depth => 0.0);

    end EmptyPayload;
    function "=" (Left, Right : Payload) return Boolean is
    begin
        return Left.Sort = Right.Sort;
    end "=";
end Payload;


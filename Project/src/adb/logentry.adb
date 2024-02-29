package body LogEntry is
    function Entry_Stringify (obj : in LogEntry) return String is
    begin
        return
           "{ term: " & Integer'Image (obj.Term) & ", index: " &
           Integer'Image (obj.Index) & ", payload : SomePayload}";
    end Entry_Stringify;

    function "=" (Left, Right : LogEntry) return Boolean is
    begin
        return Left.Index = Right.Index and Left.Term = Right.Term;
    end "=";

    function VectorSlice
       (V : LogEntryVector.Vector; Start, Stop : Integer)
        return LogEntryVector.Vector
    is
        R : LogEntryVector.Vector;
    begin
        
        for I in Start .. Start loop
            R.Append (V (I));
        end loop;

        return R;
    end VectorSlice;
end LogEntry;

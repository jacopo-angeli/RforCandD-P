package body LogEntry is
    function Entry_Stringify (obj : in LogEntry) return String is
    begin
        return
           "{ term: " & Integer'Image (obj.Term) & ", index: " &
           Integer'Image (obj.Index) & ", payload : SomePayload}";
    end Entry_Stringify;

    function "=" (Left, Right : LogEntry) return Boolean is
    begin
        return
           Left.Index = Right.Index and Left.State = Right.State and
           Left.Term = Right.Term;
    end "=";

    procedure Set_State(obj : in out LogEntry; state : LogEntryState) is
        begin
            obj.State:=state;
        end Set_State;

    function Get_State (obj : in out LogEntry) return LogEntryState is
        begin
            return obj.State;
        end Get_State;
end LogEntry;

package body LogEntry is
    function Entry_Stringify(obj: in LogEntry) return String is
    begin
        return "{ term: " & Integer'Image(obj.Term) & ", index: " & Integer'Image(obj.Index) & ", payload : SomePayload}"; 
    end Entry_Stringify;
end LogEntry;
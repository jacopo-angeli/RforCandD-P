with Payload;

package LogEntry is
    
    type LogEntry is record
        Term: Integer;
        Index: Integer;
        peyload : Payload.Payload;
    end record;

    function Entry_Stringify(obj: in LogEntry) return String;

end LogEntry;
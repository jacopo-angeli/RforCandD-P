with Payload;

package LogEntry is
    
    type LogEntryState is (APPENDED, COMMITED);

    type LogEntry is record
        Term: Integer;
        Index: Integer;
        peyload : Payload.Payload;
        state: LogEntryState;
    end record;

    function Entry_Stringify(obj: in LogEntry) return String;

end LogEntry;
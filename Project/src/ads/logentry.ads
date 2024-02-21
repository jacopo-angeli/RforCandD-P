with Payload;

package LogEntry is

    type LogEntryState is (APPENDED, COMMITTED);

    type LogEntry is record
        Term    : Integer;
        Index   : Integer;
        Peyload : Payload.Payload;
        State   : LogEntryState := APPENDED;
    end record;

    function Entry_Stringify (obj : in LogEntry) return String;
    function "=" (Left, Right : LogEntry) return Boolean;
    procedure Set_State(obj : in out LogEntry; state : LogEntryState);

end LogEntry;

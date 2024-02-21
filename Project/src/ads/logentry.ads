with Payload;

package LogEntry is

    type LogEntryState is (APPENDEDD, COMMITED);

    type LogEntry is record
        Term    : Integer;
        Index   : Integer;
        Peyload : Payload.Payload;
        State   : LogEntryState := APPENDEDD;
    end record;

    function Entry_Stringify (obj : in LogEntry) return String;
    function "=" (Left, Right : LogEntry) return Boolean;
    procedure Set_State(obj : in out LogEntry; state : LogEntryState);
    function Get_State (obj : in out LogEntry) return LogEntryState;

end LogEntry;

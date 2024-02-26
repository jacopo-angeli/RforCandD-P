with Payload;

package LogEntry is


    type LogEntry is record
        Term    : Integer;
        Index   : Integer;
        Peyload : Payload.Payload;
    end record;

    function Entry_Stringify (obj : in LogEntry) return String;
    function "=" (Left, Right : LogEntry) return Boolean;

end LogEntry;

with LogEntry;
package Message is

    --  Interface
    type Message is abstract tagged record
        Term      : Integer;
        Sender_Id : Integer;
    end record;

    function Message_Stringify (Msg : in Message) return String is abstract;

    --  Subtypes
    type Heartbeat is new Message with null record;
    function Message_Stringify (Msg : in Heartbeat) return String;

    type Commited is new Message with null record;
    function Message_Stringify (Msg : in Commited) return String;

    type Commit is new Message with null record;
    function Message_Stringify (Msg : in Commit) return String;

    type Appended is new Message with null record;
    function Message_Stringify (Msg : in Appended) return String;

    type LogOutdated is new Message with record
        LogEntri : LogEntry.LogEntry;
    end record;
    function Message_Stringify (Msg : in Appended) return String;

    type Candidated is new Message with null record;
    function Message_Stringify (Msg : in Candidated) return String;

    type Vote is new Message with null record;
    function Message_Stringify (Msg : in Vote) return String;

    type AppendEntry is new Message with record
        LogEntri : LogEntry.LogEntry;
    end record;
    function Message_Stringify (Msg : in AppendEntry) return String;

end Message;

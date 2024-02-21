with LogEntry;
with Ada.Containers;
package Message is

    --  Interface
    type Message is abstract tagged record
        Term        : Integer;
        Sender_Id   : Integer;
        Log_length  : Integer;
    end record;

    function Message_Stringify (Msg : in Message) return String is abstract;

    -- SUBTYPES --
    
    -- LEADER --
    
    type ClientOperation is new Message with null record;
    function Message_Stringify (Msg : in ClientOperation) return String;
    
    type Committed is new Message with null record;
    function Message_Stringify (Msg : in Committed) return String;
    
    type Appended is new Message with null record;
    function Message_Stringify (Msg : in Appended) return String;

    type LogOutdated is new Message with record
        LogEntri : LogEntry.LogEntry;
    end record;
    function Message_Stringify (Msg : in LogOutdated) return String;
    
    
    -- FOLLOWER --
    
    type Candidated is new Message with null record;
    function Message_Stringify (Msg : in Candidated) return String;

    type Commit is new Message with record
        LogEntri : LogEntry.LogEntry;
    end record;
    function Message_Stringify (Msg : in Commit) return String;
    
    type AppendEntry is new Message with record
        LogEntri : LogEntry.LogEntry;
    end record;
    function Message_Stringify (Msg : in AppendEntry) return String;
    

    -- CANDIDATE --

    type Vote is new Message with null record;
    function Message_Stringify (Msg : in Vote) return String;


    -- COMMON --
    
    type Heartbeat is new Message with null record;
    function Message_Stringify (Msg : in Heartbeat) return String;

end Message;

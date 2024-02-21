with Ada.Text_IO; use Ada.Text_IO;
with LogEntry;    use LogEntry;

package body Message is

    function Message_Stringify (Msg : in Heartbeat) return String is
    begin
        return "Messaggio Heartbeat";
    end Message_Stringify;

    function Message_Stringify (Msg : in Committed) return String is
    begin
        return "Messaggio Committed";
    end Message_Stringify;

    function Message_Stringify (Msg : in Commit) return String is
    begin
        return "Messaggio Commit per entry:" & Entry_Stringify (Msg.LogEntri);
    end Message_Stringify;

    function Message_Stringify (Msg : in Appended) return String is
    begin
        return "Messaggio Appended";
    end Message_Stringify;

    function Message_Stringify (Msg : in LogOutdated) return String is
    begin
        return "Messaggio LogOutdated";
    end Message_Stringify;

    function Message_Stringify (Msg : in Candidated) return String is
    begin
        return "Messaggio Candidated";
    end Message_Stringify;

    function Message_Stringify (Msg : in Vote) return String is
    begin
        return "Messaggio Vote";
    end Message_Stringify;

    function Message_Stringify (Msg : in AppendEntry) return String is
    begin
        return
           "Messaggio AppendEntry con entry:" & Entry_Stringify (Msg.LogEntri);
    end Message_Stringify;

    function Message_Stringify (Msg : in ClientOperation) return String is
    begin
        return "Messaggio ClientOperation";
    end Message_Stringify;

end Message;

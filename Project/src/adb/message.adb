with Ada.Text_IO; use Ada.Text_IO;
with LogEntry;    use LogEntry;

package body Message is

    function Message_Stringify (Msg : in AppendEntry) return String is
    begin
        return
           "Messaggio AppendEntry";
    end Message_Stringify;
    function Message_Stringify (Msg : in AppendEntryResponse) return String is
    begin
        return "Messaggio AppendEntryResponse;";
    end Message_Stringify;

    function Message_Stringify (Msg : in RequestVote) return String is
    begin
        return "Messaggio RequestVote;";
    end Message_Stringify;
    function Message_Stringify (Msg : in RequestVoteResponse) return String is
    begin
        return "Messaggio RequestVoteResponse;";
    end Message_Stringify;

    function Message_Stringify (Msg : in ClientRequest) return String is
    begin
        return "Messaggio ClientRequest;";
    end Message_Stringify;
    function Message_Stringify (Msg : in ClientResponse) return String is
    begin
        return "Messaggio ClientResponse;";
    end Message_Stringify;
end Message;

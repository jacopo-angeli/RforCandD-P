with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
package Config is
    ------------------------------------------------- FAILURE
    --  Node Failure Rate per 10 seconds
    NodeFR  : Float     := 1.0;
    --  Average Node resume times
    NodeART : Time_Span := Seconds (0);
    --  Network Failure Rate
    NetFR   : Float     := 0.0;

    ------------------------------------------------- LOGGER
    --  Append_File(Append) or Out_File (Rewrite)
    DBLogFileType : File_Mode := Out_File;

    ------------------------------------------------- TIMEOUTS
    --  MinElectionTimeoutDurationInMillisecond
    MinETD : Integer := 300;
    --  MaxElectionTimeoutDurationInMillisecond
    MaxETD : Integer := 600;
    --  MinHeartbeatDurationInMillisecond
    MinHTD : Integer := 150;
    --  MaxHeartbeatDurationInMillisecond
    MaxHTD : Integer := 300;
end Config;

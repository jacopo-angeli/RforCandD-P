package Config is 
    -------------------------------------------------  FAILURES
    --  Network Failure Rate 
    NetFR: Float := 0.0001;
    --  Node Failure Rate Per Minutes 
    NodeFR: Float := 0.1;
    --  AverageNodeResumeTimeInSeconds 
    ANRT: Duration := 5.0;
    
    ------------------------------------------------- TIMEOUTS
    --  MinElectionTimeoutDurationInMillisecond 
    MinETD: Integer := 300;
    --  MaxElectionTimeoutDurationInMillisecond 
    MaxETD: Integer := 600;
    --  MinHeartbeatDurationInMillisecond
    MinHTD : Integer := 150;
    --  MaxHeartbeatDurationInMillisecond 
    MaxHTD: Integer := 300;
end Config;
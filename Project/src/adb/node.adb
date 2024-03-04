with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;

with Message;
with Logger;
with LogEntry;
with Payload;

package body Node is
    use Ada.Text_IO;
    use Ada.Real_Time;
    use Ada.Strings.Fixed;
    use Message;
    use Ada.Containers;
    use Payload;
    use Ada.Strings.Unbounded;
    use Ada.Numerics.Elementary_Functions;

    task body Node is
        NodesNumber : Integer           := Integer (Net.all.Length / 2);
        Self        : aliased NodeState := NodeStateInit (NodesNumber);

        TimeSpanFromLastQuakeGeneration : Time := Clock;

        --  Logger
        LogEntryFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);
        LogFileName      : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

        procedure CrashSimulator is
        begin
            if (Paused.all) then
                Put_Line (Boolean'Image (Paused.all));
            end if;
            if Paused.all then
                Logger.Log (LogFileName, "Node crashed.");
                while Paused.all loop
                    delay 1.0;
                end loop;
                --  Clear of all the received message while in crash state
                Queue.Clear (Net.all (Id).all);
                --  Current type to FOLLOWER
                Self.CurrentType         := FOLLOWER;
                Self.LastPacketTimestamp := Clock;
                Logger.Log (LogFileName, "Node up.");
            end if;
        end CrashSimulator;

        procedure QuakeSimulation is
        begin
            declare

                TimeSpanFromLastQuake : Time_Span :=
                   Clock - Self.LastMoonquakeTimestamp;

                function ProbE (x : Float) return Float is
                    LambdaX : Float;
                begin
                    LambdaX := 0.01 * (x / 10.0);
                    return Exp (-LambdaX);
                end ProbE;

                Gen : Ada.Numerics.Float_Random.Generator;

                N1, N2 : Float;

                Msg          : Message.AppendEntry;
                CurrentTerm  : Integer := Self.CurrentTerm;
                LeaderId     : Integer := Id;
                PrevLogIndex : Integer;
                PrevLogTerm  : Integer;
                LogEntries   : LogEntryVector.Vector;
                LeaderCommit : Integer := Self.CommitIndex;
                Load         : Payload.Payload;

            begin

                Ada.Numerics.Float_Random.Reset (Gen);

                if (Clock - TimeSpanFromLastQuakeGeneration) > Seconds (2) then

                    N1 := Ada.Numerics.Float_Random.Random (Gen);
                    N2 := ProbE (Float (To_Duration (TimeSpanFromLastQuake)));

                    if N1 > N2 then

                        if Self.Log.Is_Empty then
                            PrevLogIndex := 0;
                            PrevLogTerm  := Self.CurrentTerm;
                        else
                            PrevLogIndex :=
                               Self.Log (Self.Log.Last_Index).Index;
                            PrevLogTerm := Self.Log (Self.Log.Last_Index).Term;
                        end if;

                        Load := Payload.RandomPayload;
                        LogEntries.Append
                           (LogEntry.LogEntry'
                               (Self.CurrentTerm, PrevLogIndex + 1, Load));

                        Msg :=
                           Message.AppendEntry'
                              (CurrentTerm,--
                               LeaderId,--
                               PrevLogIndex, --
                               PrevLogTerm, --
                               LogEntries,--
                               LeaderCommit);
                        if Self.CurrentType /= LEADER then
                            Respond (Net, Msg, Self.CurrentLeader);
                            Self.LastMoonquakeTimestamp := Clock;
                        else 
                            Net.Append(Msg);
                        end if;

                    end if;

                    TimeSpanFromLastQuakeGeneration := Clock;

                end if;

            end;
        end QuakeSimulation;

    begin

        Logger.Log (LogFileName, "Node started.");

        loop
            select
                accept Request
                   (Msg      : in     Message.ClientRequest;
                    Response :    out Message.ClientResponse)
                do
                    Logger.Log (LogFileName, "Client request received.");
                    Response :=
                       HandleClientRequest (Id, Net, Self'Access, Msg);
                end Request;
            or
                delay 0.0;

                --  Crash simulation
                CrashSimulator;

                --  Sensed Quake
                    QuakeSimulation;
                
                --  Message handle
                while not Queue.Is_Empty (Net.all (id).all) loop
                    HandleMessage
                       (Id,--
                        Net,--
                        Self'Access,--
                        Queue.Dequeue (Net.all (Id).all));

                    --  TimeoutMangment
                    TimeoutManagment (Id, Net, Self'Access);
                end loop;

                --  TimeoutMangment
                TimeoutManagment (Id, Net, Self'Access);

            end select;
        end loop;
    end Node;

    --------------------------------------------------------------------------- FUNCTIONS

    function NodeStateInit (NumberOfNodes : Integer) return NodeState is
        L          : aliased LogEntryVector.Vector;
        DB         : aliased PayloadVector.Vector;
        T1, T2     : Time_Span;
        NextIndex  : IntegerVector.Vector;
        MatchIndex : IntegerVector.Vector;
        package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
        Gen : Integer_Random.Generator;
    begin
        Integer_Random.Reset (Gen);

        -- Timeout duration in milliseconds
        T1 := Milliseconds (Integer_Random.Random (Gen) mod 300 + 300);
        T2 := Milliseconds (Integer_Random.Random (Gen) mod 150 + 150);

        NextIndex.Append (1, Ada.Containers.Count_Type (NumberOfNodes));
        MatchIndex.Append (0, Ada.Containers.Count_Type (NumberOfNodes));

        return
           NodeState'
              (CurrentTerm              => 0, --
               VotedFor                 => -1, --
               Log                      => L, --
               DB                       => DB,--
               CommitIndex              => 0,--
               LastApplied              => 0,--
               NextIndex                => NextIndex, --
               MatchIndex               => MatchIndex,--
               CurrentType              => FOLLOWER,--
               HeartbeatTimeoutDuration => T2,--
               ElectionTimeoutDuration  => T1,--
               LastMoonquakeTimestamp   => Clock,--
               CandidationTimestamp     => Clock, --
               CurrentLeader            => -1,--
               VotesCounter             => 0,--
               LastPacketTimestamp      => Clock);

    end NodeStateInit;

    function StateToString (S : in NodeState) return String is
    begin
        return
           "(" & "Term:" & Integer'Image (S.CurrentTerm) & ", VotedFor: " &
           Integer'Image (S.VotedFor) & ", CommitIndex: " &
           Integer'Image (S.CommitIndex) & ", LastApplied: " &
           Integer'Image (S.LastApplied) & ", CurrentType: " &
           NodeType'Image (S.CurrentType) & ")";
    end StateToString;

    function IntegerVectorInfimum (V : in IntegerVector.Vector) return Integer
    is

        R : Integer := Integer'Last;

    begin

        for I in V.First_Index .. V.Last_Index loop
            R := Integer'Min (R, V (I));
        end loop;

        return R;

    end IntegerVectorInfimum;

    function EqualityCount
       (V : in IntegerVector.Vector; E : Integer) return Integer
    is

        C : Integer := 0;

    begin

        for I in V.First_Index .. V.Last_Index loop
            if V (I) = E then
                C := C + 1;
            end if;
        end loop;

        return C;

    end EqualityCount;

    function HandleClientRequest
       (Id   : Integer; --
        Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.ClientRequest) return Message.ClientResponse
    is
        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

        function LeaderBehaviour return Message.ClientResponse is

            CurrentTerm  : Integer := Self.all.CurrentTerm;
            PrevLogIndex : Integer;
            PrevLogTerm  : Integer;
            LogEntries   : LogEntryVector.Vector;
            LeaderCommit : Integer := Self.all.CommitIndex;

            NewEntry : LogEntry.LogEntry :=
               LogEntry.LogEntry'
                  (CurrentTerm, Self.all.NextIndex (Id), Msg.Peyload);

            ToBroadcast : Message.AppendEntry;

        begin
            if Self.all.Log.Is_Empty then
                PrevLogIndex := 0;
                PrevLogTerm  := CurrentTerm;
            else
                PrevLogIndex := Self.all.Log (Self.all.Log.Last_Index).Index;
                PrevLogTerm  := Self.all.Log (Self.all.Log.Last_Index).Term;
            end if;

            LogEntries.Append (NewEntry);
            ToBroadcast :=
               Message.AppendEntry'
                  (CurrentTerm,--
                   Id,--
                   PrevLogIndex,--
                   PrevLogTerm,--
                   LogEntries,--
                   LeaderCommit);

            --  Appends the command to its log as a new entry, then is-
            --  sues AppendEntries RPCs in parallel to each of the other
            --  servers to replicate the entry. When the entry has been
            --  safely replicated (as described below), the leader applies
            --  the entry to its state machine and returns the result of that
            --  execution to the client

            --  Append Entry in leader log
            Self.all.Log.Append (NewEntry);
            --  Update NextIndex(Id)
            Self.all.NextIndex (Id) := Self.all.NextIndex (Id) + 1;
            --  Restore of AppendedCounter in order to detect when to respond to the client
            --  Self.all.AppendedCounter := 0;
            --  Broadcast AppendEntry
            Broadcast (Id, Net, ToBroadcast);

            declare

                ExpectedNextIndex : Integer := Self.all.NextIndex (Id);
                NetLenght         : Integer := Integer (Net.all.Length / 2);

            begin
                --  Repeat till at least N nodes has NextIndex synced, whit N=Net.Lenght\2
                while not
                   (EqualityCount (Self.all.NextIndex, ExpectedNextIndex) >
                    Integer (NetLenght / 2))
                loop
                    --------------------------------------------------------------
                    -- The point is that after a broadcast of the appendEntry   --
                    -- we have three possibility:                               --
                    --  - Follower respond true: Then NextIndex(FollowerId)     --
                    --    will be equal to NextIdex(Leader);                    --
                    --  - Follower respond false: Then leader keep send Entries --
                    --    till NextIndex(Follower) will be equal to             --
                    --    NextIndex(Leader);                                    --
                    --  - Follower doesent respond: Then leader keep sending    --
                    --    appendEntry request because it can respond to the     --
                    --    client only if the majority of the cluster appended   --
                    --    the entry;                                            --
                    --  - Leader crash: It will not respond to the client and   --
                    --    eventually all the follower that appended the entry   --
                    --    will delete it.                                       --
                    --------------------------------------------------------------
                    while not Queue.Is_Empty (Net.all (id).all) loop

                        HandleMessage
                           (Id,--
                            Net,--
                            Self,--
                            Queue.Dequeue (Net.all (Id).all));

                    end loop;
                end loop;
                return
                   Message.ClientResponse'
                      (True, To_Unbounded_String ("Everything fine."));
            end;
        end LeaderBehaviour;

        function FollowerBehaviour return Message.ClientResponse is
        begin
            --  Redirect the client call to Leader
            --  The client doesn't know that it's communicating with a follower
            --  Use respond function to send the client request to the leader
            Respond
               (Net => Net, Msg => Msg, Receiver => Self.all.CurrentLeader);
            -- loop until I recieve a ClientResponse and then return that
            while not Queue.Is_Empty (Net.all (id).all) loop
                declare
                    Msg : Message.Message'Class :=
                       Queue.Dequeue (Net.all (Id).all);
                begin
                    if (Msg in Message.ClientResponse'Class) then
                        return Message.ClientResponse (Msg);
                    end if;
                    HandleMessage
                       (Id,--
                        Net,--
                        Self,--
                        Queue.Dequeue (Net.all (Id).all));
                end;
            end loop;
            return Message.ClientResponse'(False, To_Unbounded_String (""));
            --  TODO CHANGE EXIT METHOD (using a timer and a flag)
        end FollowerBehaviour;
    begin
        case Self.all.CurrentType is
            when LEADER =>
                return LeaderBehaviour;
            when others =>
                return FollowerBehaviour;
        end case;
    end HandleClientRequest;

    --------------------------------------------------------------------------- PROCEDURES

    procedure Broadcast
       (SelfId : Integer; --
        Net    : access QueueVector.Vector;--
        Msg    : Message.Message'Class)
    is
    begin
        for I in Net.all.First_Index .. Net.all.Last_Index loop
            if I /= SelfId then
                Queue.Enqueue (net.all (I).all, Msg);
            end if;
        end loop;
    end Broadcast;

    procedure Respond
       (Net      : access QueueVector.Vector;--
        Msg      : Message.Message'Class;--
        Receiver : Integer)
    is
    begin
        Queue.Enqueue (net.all (Receiver).all, Msg);
    end Respond;

    procedure HandleMessage
       (Id   : Integer; --
        Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.Message'Class)
    is

        procedure AllServerRule (MessageTerm : Integer) is
        begin
            --  All Servers:
            --    • If RPC request or response contains term T > currentTerm:
            --      set currentTerm = T, convert to follower (§5.1)
            if MessageTerm > Self.all.CurrentTerm then
                Self.all.CurrentTerm := MessageTerm;
                Self.all.VotedFor    := -1;
            end if;
        end AllServerRule;

    begin

        if Msg in Message.AppendEntry'Class then
            AllServerRule (Message.AppendEntry (Msg).Term);
            HandleAppendEntry (Id, Net, Self, Message.AppendEntry (Msg));

        elsif Msg in Message.AppendEntryResponse'Class then
            AllServerRule (Message.AppendEntryResponse (Msg).Term);
            HandleAppendEntryResponse
               (Id, Net, Self, Message.AppendEntryResponse (Msg));

        elsif Msg in Message.RequestVote'Class then
            AllServerRule (Message.RequestVote (Msg).Term);
            HandleRequestVote (Id, Net, Self, Message.RequestVote (Msg));

        elsif Msg in Message.RequestVoteResponse'Class then
            AllServerRule (Message.RequestVoteResponse (Msg).Term);
            HandleRequestVoteResponse
               (Id, Net, Self, Message.RequestVoteResponse (Msg));

        end if;
    end HandleMessage;

    procedure HandleAppendEntry
       (Id   : Integer; --
        Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.AppendEntry)
    is
        --  Logger
        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

        procedure FollowerBehaviour is
            MessageLogEntries : LogEntryVector.Vector := Msg.LogEntries;

            MessageTerm              : Integer := Msg.Term;
            MessagePrevLogIndex      : Integer := Msg.PrevLogIndex;
            MessagePrevLogTerm       : Integer := Msg.PrevLogTerm;
            MessageLeaderCommitIndex : Integer := Msg.LeaderCommit;
            MessageLeaderId          : Integer := Msg.LeaderId;
        begin

            if MessageTerm > Self.all.CurrentTerm then
                Self.all.CurrentTerm := MessageTerm;
                Self.all.VotedFor    := -1;
            end if;

            --  1. Reply false if term < currentTerm (§5.1)
            if MessageTerm < Self.all.CurrentTerm then
                Logger.Log
                   (LogFileName,
                    "Discarded message with lower term received.");
                Respond
                   (Net, --
                    Message.AppendEntryResponse'
                       (Self.all.CurrentTerm,--
                        Id,--
                        False),--
                    MessageLeaderId);
                return;
            else
                Self.all.LastPacketTimestamp := Clock;
                Self.all.CurrentLeader       := Msg.LeaderId;
            end if;

            if not Msg.LogEntries.Is_Empty then
                Logger.Log (LogFileName, "Received Append Entry");

                declare

                    Element : LogEntry.LogEntry;

                begin

                    Element := Self.all.Log (MessagePrevLogIndex);

                    --  3. If an existing entry conflicts with a new one (same index
                    --     but different terms), delete the existing entry and all that
                    --     follow it (§5.3)
                    if Element.Term /= MessagePrevLogTerm then
                        Self.all.Log.Delete
                           (Element.Index,
                            Self.all.Log.Length -
                            Ada.Containers.Count_Type (Element.Index));
                    end if;

                    --  4. Append any new entries not already in the log
                    --  LogEntryVector.Append
                    --     (Self.all.Log, MessageLogEntry);

                    --  5. If leaderCommit > commitIndex, set commitIndex =
                    --     min(leaderCommit, index of last new entry)
                    Self.all.CommitIndex :=
                       Integer'Min
                          (MessageLeaderCommitIndex,
                           MessageLogEntries (MessageLogEntries.Last_Index)
                              .Index);

                    Respond
                       (Net,
                        Message.AppendEntryResponse'
                           (Self.all.CurrentTerm,--
                            Id,--
                            True),--
                        MessageLeaderId);

                exception

                    --  No element in Log at index MessagePrevLogIndex
                    when others =>
                        Logger.Log (LogFileName, "Empty Log");

                        --  If MessagePrevLogIndex = 0 append the message else return false
                        --  else respond false
                        if MessagePrevLogIndex = 0 then

                            for E of MessageLogEntries loop
                                Self.all.Log.Append (E);
                            end loop;

                            Self.all.CommitIndex :=
                               Integer'Min
                                  (MessageLeaderCommitIndex,
                                   Self.all.Log.Last_Index);

                            Respond
                               (Net, --
                                Message.AppendEntryResponse'
                                   (Self.all.CurrentTerm,--
                                    Id,--
                                    True),--
                                MessageLeaderId);
                            Logger.Log (LogFileName, "Append successful");

                        else

                            Respond
                               (Net, --
                                Message.AppendEntryResponse'
                                   (Self.all.CurrentTerm,--
                                    Id,--
                                    False),--
                                MessageLeaderId);
                            Logger.Log (LogFileName, "Append fail");

                        end if;

                end;

            end if;

        end FollowerBehaviour;

        procedure CandidateBehaviour is
            MessageLogEntry : LogEntryVector.Vector := Msg.LogEntries;

            MessageTerm              : Integer := Msg.Term;
            MessagePrevLogIndex      : Integer := Msg.PrevLogIndex;
            MessageLeaderCommitIndex : Integer := Msg.LeaderCommit;
            MessageLeaderId          : Integer := Msg.LeaderId;
        begin

            Logger.Log
               (LogFileName,
                "Candidate and received message AppendEntry (" &
                Integer'Image (MessageTerm) & "," &
                Integer'Image (MessageLeaderId) & ")");

            --  If AppendEntries RPC received from new leader: convert to follower
            --  if MessageTerm > CurrentTerm.all then
            --     CurrentTerm.all  := MessageTerm;
            --     VotedFor.all     := -1;
            --     CurrentState.all := FOLLOWER;
            --     Logger.Log
            --       (File_Name => LogFileName,
            --        Content   =>
            --          "AppendEntries RPC received from new leader: convert to follower.");
            --     return;
            --  end if;
            Self.all.CurrentType         := FOLLOWER;
            Self.all.LastPacketTimestamp := Clock;

        end CandidateBehaviour;

        procedure LeaderBehaviour is

            MessageTerm     : Integer := Msg.Term;
            MessageLeaderId : Integer := Msg.LeaderId;
            NetLenght       : Integer := Integer (Net.all.Length);

        begin

            Put_Line
               (Integer'Image (MessageTerm) & " " &
                Integer'Image (Self.all.CurrentTerm));
            --  TODO : Change
            if MessageTerm > Self.all.CurrentTerm then
                Self.all.CurrentTerm         := MessageTerm;
                Self.all.CurrentType         := FOLLOWER;
                Self.all.LastPacketTimestamp := Clock;
            else

                Logger.Log (LogFileName, "AppendEntry from Follower");
            end if;

        end LeaderBehaviour;

    begin
        case Self.all.CurrentType is
            when FOLLOWER =>

                --  1. Reply false if term < currentTerm (§5.1)
                --  2. Reply false if log doesn’t contain an entry at prevLogIndex
                --     whose term matches prevLogTerm (§5.3)
                --  3. If an existing entry conflicts with a new one (same index
                --     but different terms), delete the existing entry and all that
                --     follow it (§5.3)
                --  4. Append any new entries not already in the log
                --  5. If leaderCommit > commitIndex, set commitIndex =
                --     min(leaderCommit, index of last new entry)
                FollowerBehaviour;

            when CANDIDATE =>
                --  If the leader’s term (included in its RPC) is at least
                --  as large as the candidate’s current term, then the candidate
                --  recognizes the leader as legitimate and returns to follower
                --  state.
                CandidateBehaviour;

            when LEADER =>
                LeaderBehaviour;

        end case;
    end HandleAppendEntry;

    procedure HandleAppendEntryResponse
       (Id   : Integer; --
        Net  : access QueueVector.Vector;--
        Self : access NodeState;--
        Msg  : Message.AppendEntryResponse)
    is

        --  Logger
        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

    begin
        case Self.all.CurrentType is
            when FOLLOWER =>
                null;
            when CANDIDATE =>
                null;
            when LEADER =>
                declare

                    MessageSender  : Integer := Msg.Sender;
                    MessageSuccess : Boolean := Msg.Success;
                    MessageTerm    : Integer := Msg.Term;

                    NetLenght : Integer := Integer (Net.all.Length);

                begin

                    if not MessageSuccess then

                        --  After a rejection, the leader decrements
                        --  nextIndex and retries the AppendEntries RPC.
                        Self.all.NextIndex (MessageSender) :=
                           Self.all.NextIndex (MessageSender) - 1;

                        declare
                            Term         : Integer := Self.all.CurrentTerm;
                            LeaderId     : Integer := Id;
                            PrevLogIndex : Integer :=
                               Self.all.NextIndex (MessageSender) - 1;
                            PrevLogTerm  : Integer :=
                               Self.all.Log (PrevLogIndex).Term;
                            LogEntries   : LogEntryVector.Vector;
                            LeaderCommit : Integer := Self.all.CommitIndex;
                        begin

                            LogEntries :=
                               LogEntry.VectorSlice
                                  (Self.all.Log,--
                                   PrevLogIndex + 1,--
                                   Self.all.Log.Last_Index);

                            Respond
                               (Net,
                                Message.AppendEntry'
                                   (Term,--
                                    LeaderId,--
                                    PrevLogIndex,--
                                    PrevLogTerm,--
                                    LogEntries,--
                                    LeaderCommit),
                                MessageSender);
                        end;

                    else
                        --  Entry successful
                        --  AppendCounter ++
                        Self.all.NextIndex (MessageSender) :=
                           Self.all.NextIndex (MessageSender) + 1;
                        Logger.Log
                           (LogFileName,
                            "Node " & Integer'Image (MessageSender) &
                            " appended successfully. NextIndex(" &
                            Integer'Image (MessageSender) & " ) = " &
                            Integer'Image
                               (Self.all.NextIndex (MessageSender)));
                    end if;

                end;
        end case;
    end HandleAppendEntryResponse;

    procedure HandleRequestVote
       (Id   : Integer; --
        Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.RequestVote)
    is
        --  Logger
        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);
    begin
        Self.all.LastPacketTimestamp := Clock;
        case Self.all.CurrentType is
            when FOLLOWER =>
                declare

                    MessageTerm         : Integer := Msg.Term;
                    MessageCandidateId  : Integer := Msg.CandidateId;
                    MessageLastLogIndex : Integer := Msg.LastLogIndex;
                    MessageLastLogTerm  : Integer := Msg.LastLogTerm;

                begin

                    Logger.Log
                       (File_Name => LogFileName,
                        Content   =>
                           "(" & Integer'Image (Self.all.CurrentTerm) &
                           ") : Request vote from " &
                           Integer'Image (MessageCandidateId) & " with term " &
                           Integer'Image (MessageTerm));

                    --  1. Reply false if term < currentTerm (§5.1)
                    if MessageTerm < Self.all.CurrentTerm then
                        Respond
                           (Net,
                            Message.RequestVoteResponse'
                               (Self.all.CurrentTerm, False),
                            MessageCandidateId);
                        return;
                    end if;

                    declare
                        LastLogElementIndex : Integer;
                        LastLogElementTerm  : Integer;
                    begin

                        if Self.all.Log.Is_Empty then
                            LastLogElementIndex := 0;
                            LastLogElementTerm  := Self.all.CurrentTerm;
                        else
                            LastLogElementIndex :=
                               Self.all.Log (Self.all.Log.Last_Index).Index;
                            LastLogElementTerm  :=
                               Self.all.Log (Self.all.Log.Last_Index).Term;
                        end if;

                        --  2. If votedFor is null or candidateId, and candidate’s log is at
                        --     least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)
                        if (Self.all.VotedFor = -1 or
                            Self.all.VotedFor = MessageCandidateId) and
                           (LastLogElementIndex <= MessageLastLogIndex and
                            LastLogElementTerm <= MessageLastLogTerm)
                        then
                            Respond
                               (Net,
                                Message.RequestVoteResponse'
                                   (Self.all.CurrentTerm, --
                                    True),
                                MessageCandidateId);

                            Self.all.VotedFor            := MessageCandidateId;
                            Self.all.LastPacketTimestamp := Clock;

                            Logger.Log
                               (LogFileName,
                                "Vote granted to " &
                                Integer'Image (MessageCandidateId));

                            return;

                        else
                            Respond
                               (Net      => Net,
                                Msg      =>
                                   Message.RequestVoteResponse'
                                      (Self.all.CurrentTerm,--
                                       False),
                                Receiver => MessageCandidateId);
                        end if;

                    end;
                end;
            when CANDIDATE =>
                null;
            when LEADER =>
                null;
        end case;
    end HandleRequestVote;

    procedure HandleRequestVoteResponse
       (Id   : Integer; --
        Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.RequestVoteResponse)
    is

        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

        procedure LeaderBehaviour is
        begin
            null;
        end LeaderBehaviour;
        procedure CandidateBehaviour is
            MessageSuccess : Boolean := Msg.VoteGranted;
            MessageTerm    : Integer := Msg.Term;

            NetLenght : Integer := Integer (Net.all.Length);

        begin
            Logger.Log
               (LogFileName,
                "Voted received:" & Boolean'Image (MessageSuccess));

            if MessageSuccess then
                Self.all.VotesCounter := Self.all.VotesCounter + 1;

                if Self.all.VotesCounter > Integer (NetLenght / 2) then
                    Self.all.CurrentType := LEADER;

                    --  When a leader first comes to power,
                    --  it initializes all nextIndex values to the index just after the
                    --  last one in its log (11 in Figure 7
                    Self.all.NextIndex.Clear;
                    Self.all.NextIndex.Append
                       (Integer (Self.all.Log.Last_Index) + 1, Net.Length);

                    Logger.Log (LogFileName, "Now leader.");
                end if;

            end if;
        end CandidateBehaviour;
        procedure FollowerBehaviour is
        begin
            null;
        end FollowerBehaviour;
    begin
        case Self.all.CurrentType is
            when LEADER =>
                LeaderBehaviour;

            when CANDIDATE =>
                CandidateBehaviour;

            when FOLLOWER =>
                FollowerBehaviour;
        end case;
    end HandleRequestVoteResponse;

    procedure TimeoutManagment
       (Id   : Integer;--
        Net  : access QueueVector.Vector;--
        Self : access NodeState)
    is

        TimeSpanFromLastHeartbeat : Time_Span :=
           Clock - Self.all.LastPacketTimestamp;
        TimeSpanFromCandidation   : Time_Span :=
           Clock - Self.all.CandidationTimestamp;

        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

        procedure AllServerRule is
        begin
            --  All Servers:
            --    • If commitIndex > lastApplied: increment lastApplied, apply
            --      log[lastApplied] to state machine (§5.3)
            declare
                CommitIndex : Integer := Self.all.CommitIndex;
                LastApplied : Integer := Self.all.LastApplied;
            begin
                if CommitIndex > LastApplied then
                    Self.all.LastApplied := CommitIndex;
                    --  Apply(Self.all.Log(LastApplied));
                end if;
            end;
        end AllServerRule;

        procedure LeaderBehaviour is
        begin
            --  If there exists an N such that N > commitIndex, a majority
            --  of matchIndex[i] ≥ N, and log[N].term == currentTerm:
            --  set commitIndex = N (§5.3, §5.4).

            --  Find the min of MatchIndex
            --  If it's > commitIndex them update CommitIndex
            declare
                N : Integer := IntegerVectorInfimum (Self.all.MatchIndex);
            begin
                Self.all.CommitIndex := Integer'Max (N, Self.all.CommitIndex);
            end;

            -- Heartbeat managment
            --   If expired send heartbeat and update LastPacketTimestamp
            if (TimeSpanFromLastHeartbeat > Self.all.HeartbeatTimeoutDuration)
            then

                declare

                    PrevLogTerm   : Integer;
                    MessageToSend : Message.AppendEntry;

                begin

                    if Self.all.Log.Is_Empty then
                        PrevLogTerm := Self.all.CurrentTerm;
                    else
                        PrevLogTerm :=
                           Self.all.Log (Self.all.Log.Last_Index).Term;
                    end if;

                    MessageToSend :=
                       Message.AppendEntry'
                          (Term         => Self.all.CurrentTerm,--
                           LeaderId     => Id,--
                           PrevLogIndex => Self.all.LastApplied,--
                           PrevLogTerm  => PrevLogTerm,--
                           LogEntries   => LogEntryVector.Empty_Vector,--
                           LeaderCommit => Self.all.CommitIndex);

                    Broadcast (Id, Net, MessageToSend);
                    Self.all.LastPacketTimestamp := Clock;

                exception
                    when others =>
                        Logger.Log
                           (LogFileName,--
                            "Exception on timeout managment.");

                end;

            end if;
        end LeaderBehaviour;

        procedure CandidateBehaviour is
        begin
            --  • If election timeout elapses: start new election
            if TimeSpanFromCandidation > Self.all.ElectionTimeoutDuration then

                --  TODO : Think of change all this if content to a CurrentType := FOLLOWER
                Self.all.CurrentTerm          := Self.all.CurrentTerm + 1;
                Self.all.VotesCounter         := 1;
                Self.all.CandidationTimestamp := Clock;

                Broadcast
                   (Id,--
                    Net,--
                    Message.RequestVote'
                       (Term         => Self.all.CurrentTerm,--
                        CandidateId  => Id,--
                        LastLogIndex =>
                           Self.all.Log (Self.all.Log.Last_Index).Index,
                        LastLogTerm  =>
                           Self.all.Log (Self.all.Log.Last_Index).Term));

                Logger.Log (LogFileName, "Requested vote...");

            end if;
        end CandidateBehaviour;

        procedure FollowerBehaviour is
        begin
            if TimeSpanFromLastHeartbeat > Self.all.ElectionTimeoutDuration
            then

                Logger.Log
                   (File_Name => LogFileName,--
                    Content   => "Election timeout expired");

                --  • On conversion to candidate, start election:
                --      • Increment currentTerm
                --      • Vote for self
                --      • Reset election timer
                --      • Send RequestVote RPCs to all other servers
                Self.all.CurrentType          := CANDIDATE;
                Self.all.CurrentTerm          := Self.all.CurrentTerm + 1;
                Self.all.VotesCounter         := 1;
                Self.all.CandidationTimestamp := Clock;

                declare
                    Term         : Integer := Self.all.CurrentTerm;
                    LastLogIndex : Integer;
                    LastLogTerm  : Integer;
                begin

                    if Self.all.Log.Is_Empty then
                        LastLogIndex := 0;
                        LastLogTerm  := Self.all.CurrentTerm;
                    else
                        LastLogIndex :=
                           Self.all.Log (Self.all.Log.Last_Index).Index;
                        LastLogTerm  :=
                           Self.all.Log (Self.all.Log.Last_Index).Term;
                    end if;

                    Broadcast
                       (Id,--
                        Net,--
                        Message.RequestVote'
                           (Term,--
                            Id,--
                            LastLogIndex,--
                            LastLogTerm));

                end;

                Logger.Log
                   (File_Name => LogFileName,--
                    Content   => "Requested vote to other nodes on the net.");
            end if;
        end FollowerBehaviour;

    begin
        AllServerRule;

        case Self.all.CurrentType is
            when LEADER =>
                LeaderBehaviour;

            when CANDIDATE =>
                CandidateBehaviour;

            when FOLLOWER =>
                FollowerBehaviour;

        end case;

    end TimeoutManagment;

end Node;

with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;

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

    task body Node is

        Self : aliased NodeState := NodeStateInit;

        --  Log
        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

    begin

        Logger.Log (File_Name => LogFileName, Content => "Node started.");

        loop

            --  Crash simulation
            if Paused.all then
                Logger.Log (LogFileName, "Node crashed.");
                while Paused.all loop
                    delay 1.0;
                end loop;
                --  Clear of all the received message while in crash state
                Queue.Clear (Net.all (Id).all);
                --  Current type to FOLLOWER
                Self.CurrentType := FOLLOWER;
                Logger.Log (LogFileName, "Node up.");
            end if;

            --  Message handle
            while not Queue.Is_Empty (Net.all (id).all) loop
                HandleMessage
                   (Net,--
                    Self'Access,--
                    Queue.Dequeue (Net.all (Id).all));
            end loop;

            --  TimeoutMangment
            TimeoutManagment (Id, Net, Self'Access);

            delay 0.3;
            null;
        end loop;
    end Node;

    --------------------------------------------------------------------------- FUNCTIONS
    function NodeStateInit return NodeState is
        L : aliased LogEntryVector.Vector;
        T : Integer;
        package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
        Gen : Integer_Random.Generator;
    begin
        --  Append of an empty Entry to resolve the dangling access exception
        LogEntryVector.Append
           (L, LogEntry.LogEntry'(0, 1, Payload.EmptyPayload));

        -- Timeout duration in milliseconds
        T := (Integer_Random.Random (Gen) mod 10 + 3) * 1_000;

        return

           NodeState'
              (CurrentTerm              => 0, --
               VotedFor                 => -1, --
               Log                      => L, --
               CommitIndex              => 1,--
               LastApplied              => 1,--
               NextIndex                => 2, --
               MatchIndex               => 1,--
               CurrentType              => FOLLOWER,--
               HeartbeatTimeoutDuration => Integer (T / 2),--
               ElectionTimeoutDuration  => T,--
               CandidationTimestamp     => Clock, --
               AppendedCounter          => 0,--
               VotesCounter             => 0,--
               LastPacketTimestamp      => Clock);

    end NodeStateInit;

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
       (Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.Message'Class)
    is
    begin
        if Msg in Message.AppendEntry'Class then
            HandleAppendEntry (Net, Self, Message.AppendEntry (Msg));
        elsif Msg in Message.AppendEntryResponse'Class then
            HandleAppendEntryResponse
               (Net, Self, Message.AppendEntryResponse (Msg));
        elsif Msg in Message.RequestVote'Class then
            HandleRequestVote (Net, Self, Message.RequestVote (Msg));
        elsif Msg in Message.RequestVoteResponse'Class then
            HandleRequestVoteResponse
               (Net, Self, Message.RequestVoteResponse (Msg));
        end if;
    end HandleMessage;

    procedure HandleAppendEntry
       (Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.AppendEntry)
    is
    begin
        null;
    end HandleAppendEntry;

    procedure HandleAppendEntryResponse
       (Net  : access QueueVector.Vector;--
        Self : access NodeState;--
        Msg  : Message.AppendEntryResponse)
    is
    begin
        null;
    end HandleAppendEntryResponse;

    procedure HandleRequestVote
       (Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.RequestVote)
    is
    begin
        null;
    end HandleRequestVote;

    procedure HandleRequestVoteResponse
       (Net  : access QueueVector.Vector;--
        Self : access NodeState; --
        Msg  : Message.RequestVoteResponse)
    is
    begin
        null;
    end HandleRequestVoteResponse;

    procedure TimeoutManagment
       (Id   : Integer;--
        Net  : access QueueVector.Vector;--
        Self : access NodeState)
    is
        CurrentState             : NodeType := Self.all.CurrentType;
        ElectionTimeoutDuration  : Integer := Self.all.ElectionTimeoutDuration;
        HeartbeatTimeoutDuration : Integer := Self.all.ElectionTimeoutDuration;
        LastPacketTimestamp      : Time     := Self.all.LastPacketTimestamp;
        CandidationTimestamp     : Time     := Self.all.CandidationTimestamp;

        TimeSpanFromLastHeartbeat : Integer :=
           Integer (To_Duration (Clock - LastPacketTimestamp)) * 1_000;
        TimeSpanFromCandidation   : Integer :=
           Integer (To_Duration (Clock - CandidationTimestamp)) * 1_000;

        LogFileName : constant String :=
           "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);
    begin

        if (CurrentState = LEADER) then

            if (TimeSpanFromLastHeartbeat > HeartbeatTimeoutDuration) then
                -- Heartbeat expired
                -- Send heartbeat
                -- Make LastPacketTimestamp = now
                declare

                    EmptyEntry : LogEntry.LogEntry :=
                       LogEntry.LogEntry'
                          (Term    => Self.all.CurrentTerm,--
                           Index   => Self.all.LastApplied, --
                           Peyload => Payload.EmptyPayload);

                    PrevLogTerm : Integer :=
                       Self.all.Log (Self.all.LastApplied).Term;

                    MessageToSend : Message.AppendEntry :=
                       Message.AppendEntry'
                          (Term         => Self.all.CurrentTerm,--
                           LeaderId     => Id,--
                           PrevLogIndex => Self.all.LastApplied,--
                           PrevLogTerm  => PrevLogTerm,--
                           LogEntri     => EmptyEntry,--
                           LeaderCommit => Self.all.CommitIndex);

                begin

                    Broadcast (Id, Net, MessageToSend);

                end;

                Self.all.LastPacketTimestamp := Clock;
            end if;

        end if;

        if (CurrentState = FOLLOWER) then

            if (TimeSpanFromLastHeartbeat > ElectionTimeoutDuration) then

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
                Broadcast
                   (Id,--
                    Net,--
                    Message.RequestVote'
                       (Term         => Self.all.CurrentTerm,--
                        CandidateId  => Id,--
                        LastLogIndex =>
                           Self.all.Log (Self.all.Log.Last_Index).Index,--
                        LastLogTerm  =>
                           Self.all.Log (Self.all.Log.Last_Index).Term));

                Logger.Log
                   (File_Name => LogFileName,--
                    Content   => "Requested vote to other nodes on the net.");
            end if;

        end if;

        if (CurrentState = CANDIDATE) then
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
                           Self.all.Log (Self.all.Log.Last_Index).Index,--
                        LastLogTerm  =>
                           Self.all.Log (Self.all.Log.Last_Index).Term));
                Logger.Log
                   (File_Name => LogFileName, Content => "Requested vote...");
            end if;
        end if;

    end TimeoutManagment;

    --     procedure SendToId
    --       (Net      : access QueueVector.Vector; Msg : Message.Message'Class;
    --        Receiver : Integer)
    --     is
    --     begin
    --        Queue.Enqueue (net.all (Receiver).all, Msg);

    --     end SendToId;

    --     procedure HandleMessage
    --       (Net             :        access QueueVector.Vector; Id : Integer;
    --        Msg :    Message.Message'Class; LastAppendEntryTimestamp : access Time;
    --        CurrentTerm     :        access Integer; CurrentState : access State;
    --        Log : in out LogEntryVector.Vector; CommitIndex : access Integer;
    --        VotedFor        :        access Integer; VotesCounter : access Integer;
    --        TimeoutDuration :        access Integer)
    --     is
    --        LogFileName : constant String :=
    --          "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);
    --     begin

    --        case CurrentState.all is

    --           when FOLLOWER =>
    --              if Msg in AppendEntry'Class then

    --                 LastAppendEntryTimestamp.all := Clock;

    --                 --  1. Reply false if term < currentTerm (§5.1)
    --                 --  2. Reply false if log doesn’t contain an entry at prevLogIndex
    --                 --     whose term matches prevLogTerm (§5.3)
    --                 --  3. If an existing entry conflicts with a new one (same index
    --                 --     but different terms), delete the existing entry and all that
    --                 --     follow it (§5.3)
    --                 --  4. Append any new entries not already in the log
    --                 --  5. If leaderCommit > commitIndex, set commitIndex =
    --                 --     min(leaderCommit, index of last new entry)

    --                 declare
    --                    MessageLogEntry          : LogEntry.LogEntry :=
    --                      AppendEntry (Msg).LogEntri;
    --                    MessageTerm              : Integer := AppendEntry (Msg).Term;
    --                    MessagePrevLogIndex      : Integer           :=
    --                      AppendEntry (Msg).PrevLogIndex;
    --                    MessageLeaderCommitIndex : Integer           :=
    --                      AppendEntry (Msg).LeaderCommit;
    --                    MessageLeaderId : Integer := AppendEntry (Msg).LeaderId;
    --                 begin
    --                    Logger.Log
    --                      (File_Name => LogFileName,
    --                       Content   =>
    --                         "Follower and received message AppendEntry (" &
    --                         Integer'Image (MessageTerm) & "," &
    --                         Integer'Image (MessageLeaderId) & ")");

    --                    if MessageTerm > CurrentTerm.all then
    --                       CurrentTerm.all := MessageTerm;
    --                       VotedFor.all    := -1;
    --                    end if;

    --                    --  1. Reply false if term < currentTerm (§5.1)
    --                    if MessageTerm < CurrentTerm.all then
    --                       SendToId
    --                         (Net      => Net,
    --                          Msg      =>
    --                            Message.AppendEntryResponse'
    --                              (Term => CurrentTerm.all, Success => False),
    --                          Receiver => MessageLeaderId);
    --                       return;
    --                    end if;

    --                    declare
    --                       Element : LogEntry.LogEntry;
    --                    begin
    --                       Element := Log (MessagePrevLogIndex);

    --                       --  3. If an existing entry conflicts with a new one (same index
    --                       --     but different terms), delete the existing entry and all that
    --                       --     follow it (§5.3)
    --                       if Element.Term /= MessageTerm then
    --                          Log.Delete
    --                            (Element.Index,
    --                             Log.Length -
    --                             Ada.Containers.Count_Type (Element.Index));
    --                       end if;

    --                       --  4. Append any new entries not already in the log
    --                       LogEntryVector.Append (Log, MessageLogEntry);

    --                       --  5. If leaderCommit > commitIndex, set commitIndex =
    --                       --     min(leaderCommit, index of last new entry)
    --                       CommitIndex.all :=
    --                         Integer'Min
    --                           (MessageLeaderCommitIndex, MessageLogEntry.Index);

    --                       SendToId
    --                         (Net      => Net,
    --                          Msg      =>
    --                            Message.AppendEntryResponse'
    --                              (Term => CurrentTerm.all, Success => True),
    --                          Receiver => MessageLeaderId);

    --                    end;

    --                 end;

    --              elsif Msg in RequestVote'Class then

    --                 --  1. Reply false if term < currentTerm (§5.1)
    --                 --  2. If votedFor is null or candidateId, and candidate’s log is at
    --                 --     least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)

    --                 declare

    --                    MessageTerm         : Integer := RequestVote (Msg).Term;
    --                    MessageCandidateId  : Integer :=
    --                      RequestVote (Msg).CandidateId;
    --                    MessageLastLogIndex : Integer :=
    --                      RequestVote (Msg).LastLogIndex;
    --                    MessageLastLogTerm  : Integer :=
    --                      RequestVote (Msg).LastLogTerm;

    --                 begin
    --                    if MessageTerm > CurrentTerm.all then
    --                       CurrentTerm.all := MessageTerm;
    --                       VotedFor.all    := -1;
    --                    end if;

    --                    Logger.Log
    --                      (File_Name => LogFileName,
    --                       Content   =>
    --                         "(" & Integer'Image (CurrentTerm.all) &
    --                         ") : Request vote from " &
    --                         Integer'Image (MessageCandidateId) & " with term " &
    --                         Integer'Image (MessageTerm));

    --                    --  1. Reply false if term < currentTerm (§5.1)
    --                    if MessageTerm < CurrentTerm.all then
    --                       SendToId
    --                         (Net      => Net,
    --                          Msg      =>
    --                            Message.RequestVoteResponse'
    --                              (Term => CurrentTerm.all, VoteGranted => False),
    --                          Receiver => MessageCandidateId);
    --                       return;
    --                    end if;

    --                    declare
    --                       LastLogElement : LogEntry.LogEntry;
    --                    begin
    --                       LastLogElement := Log (Log.Last_Index);

    --                       --  2. If votedFor is null or candidateId, and candidate’s log is at
    --                       --     least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)
    --                       if
    --                         (VotedFor.all = -1 or
    --                          VotedFor.all = MessageCandidateId) and
    --                         (LastLogElement.Index <= MessageLastLogIndex and
    --                          LastLogElement.Term <= MessageLastLogTerm)
    --                       then
    --                          SendToId
    --                            (Net      => Net,
    --                             Msg      =>
    --                               Message.RequestVoteResponse'
    --                                 (Term => CurrentTerm.all, VoteGranted => True),
    --                             Receiver => MessageCandidateId);

    --                          VotedFor.all := MessageCandidateId;

    --                          Logger.Log
    --                            (File_Name => LogFileName,
    --                             Content   =>
    --                               "Vote granted to " &
    --                               Integer'Image (MessageCandidateId));
    --                          return;

    --                       else
    --                          SendToId
    --                            (Net      => Net,
    --                             Msg      =>
    --                               Message.RequestVoteResponse'
    --                                 (Term => CurrentTerm.all, VoteGranted => False),
    --                             Receiver => MessageCandidateId);
    --                       end if;

    --                    end;
    --                 end;

    --              else
    --                 --  TODO : Theoretically impossible
    --                 null;
    --              end if;

    --           when CANDIDATE =>
    --              if Msg in AppendEntry'Class then
    --                 --  If the leader’s term (included in its RPC) is at least
    --                 --  as large as the candidate’s current term, then the candidate
    --                 --  recognizes the leader as legitimate and returns to follower
    --                 --  state.
    --                 declare

    --                    MessageLogEntry          : LogEntry.LogEntry :=
    --                      AppendEntry (Msg).LogEntri;
    --                    MessageTerm              : Integer := AppendEntry (Msg).Term;
    --                    MessagePrevLogIndex      : Integer           :=
    --                      AppendEntry (Msg).PrevLogIndex;
    --                    MessageLeaderCommitIndex : Integer           :=
    --                      AppendEntry (Msg).LeaderCommit;
    --                    MessageLeaderId : Integer := AppendEntry (Msg).LeaderId;

    --                 begin

    --                    Logger.Log
    --                      (File_Name => LogFileName,
    --                       Content   =>
    --                         "Candidate and received message AppendEntry (" &
    --                         Integer'Image (MessageTerm) & "," &
    --                         Integer'Image (MessageLeaderId) & ")");

    --                    --  If AppendEntries RPC received from new leader: convert to follower
    --                    --  if MessageTerm > CurrentTerm.all then
    --                    --     CurrentTerm.all  := MessageTerm;
    --                    --     VotedFor.all     := -1;
    --                    --     CurrentState.all := FOLLOWER;
    --                    --     Logger.Log
    --                    --       (File_Name => LogFileName,
    --                    --        Content   =>
    --                    --          "AppendEntries RPC received from new leader: convert to follower.");
    --                    --     return;
    --                    --  end if;
    --                    CurrentState.all             := FOLLOWER;
    --                    LastAppendEntryTimestamp.all := Clock;

    --                 end;

    --              elsif Msg in RequestVoteResponse'Class then

    --                 declare

    --                    MessageSuccess : Boolean :=
    --                      RequestVoteResponse (Msg).VoteGranted;
    --                    MessageTerm    : Integer := RequestVoteResponse (Msg).Term;

    --                    NetLenght : Integer := Integer (Net.all.Length);

    --                 begin
    --                    Logger.Log
    --                      (File_Name => LogFileName,
    --                       Content   =>
    --                         "Voted received:" & Boolean'Image (MessageSuccess));
    --                    if MessageSuccess then
    --                       VotesCounter.all := VotesCounter.all + 1;

    --                       if VotesCounter.all > Integer (NetLenght / 2) then
    --                          CurrentState.all    := LEADER;
    --                          TimeoutDuration.all :=
    --                            Integer (TimeoutDuration.all / 2);

    --                          Logger.Log
    --                            (File_Name => LogFileName,
    --                             Content   => "Now LEADER...");
    --                       end if;

    --                    end if;
    --                 end;
    --              else
    --                 --  TODO : Theoretically impossible
    --                 null;
    --              end if;

    --           when LEADER =>
    --              if Msg in AppendEntryResponse'Class then
    --                 --  TODO
    --                 null;
    --              elsif Msg in AppendEntry'Class then
    --                 --  TODO
    --                 declare

    --                    MessageTerm     : Integer := AppendEntry (Msg).Term;
    --                    MessageLeaderId : Integer := AppendEntry (Msg).LeaderId;
    --                    NetLenght       : Integer := Integer (Net.all.Length);

    --                 begin

    --                    Logger.Log
    --                      (File_Name => LogFileName,
    --                       Content   =>
    --                         "Candidate and received message AppendEntry (" &
    --                         Integer'Image (MessageTerm) & "," &
    --                         Integer'Image (MessageLeaderId) & ")");

    --                    CurrentTerm.all              := MessageTerm;
    --                    CurrentState.all             := FOLLOWER;
    --                    LastAppendEntryTimestamp.all := Clock;
    --                 end;
    --                 null;
    --              else
    --                 --  TODO : Theoretically impossible
    --                 null;
    --              end if;
    --        end case;
    --     end HandleMessage;

end Node;

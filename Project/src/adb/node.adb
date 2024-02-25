--  with Ada.Text_IO;
--  with Ada.Real_Time;
--  with Ada.Strings.Fixed;
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Containers.Vectors;

--  with Message;
--  with Logger;
--  with LogEntry;
--  with Payload;

package body Node is
--     use Ada.Text_IO;
--     use Ada.Real_Time;
--     use Ada.Strings.Fixed;
--     use Message;
--     use Ada.Containers;

--     task body Node is

--       Self : NodeState := NodeState;

--        --  Log
--        LogFileName : constant String :=
--          "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

--        package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
--        Gen : Integer_Random.Generator;

--     begin
--        --  Costants initialization
--        Integer_Random.Reset (Gen);

--        --  Variable initialization for Follower Nodes
--        TimeoutDuration :=
--          (Integer_Random.Random (Gen) mod 10 + 3) *
--          1_000; -- TODO : Change to mod 151 + 150

--        LogEntryVector.Append
--          (Log, LogEntry.LogEntry'(CurrentTerm, 1, Payload.EmptyPayload));

--        loop
--           Logger.Log (File_Name => LogFileName, Content => "Node paused.");
--           while Paused.all loop
--              delay 1.0;
--           end loop;

--           --  Message handle
--           while not Queue.Is_Empty (net.all (id).all) loop
--              HandleMessage
--                (Net, Id, Queue.Dequeue (net.all (Id).all),
--                 LastAppendEntryTimestamp'Access, CurrentTerm'Access,
--                 CurrentState'Access, Log, CommitIndex'Access, VotedFor'Access,
--                 VotesCounter'Access, TimeoutDuration'Access);
--           end loop;

--           if (CurrentState = LEADER) then

--              declare
--                 Milliseconds_From_Last_Heartbeat : Integer :=
--                   Integer (To_Duration (Clock - LastAppendEntryTimestamp)) *
--                   1_000;
--              begin
--                 if
--                   (Integer (Milliseconds_From_Last_Heartbeat) > TimeoutDuration)
--                 then
--                    -- Heartbeat expired
--                    -- Send heartbeat to all the nodes
--                    -- Make Last_Heartbeat = 0
--                    -- Broadcast(Id, Net, Message.AppendEntry'(CurrentTerm,Id, LastApplied, Log(LastApplied).Term,
--                    -- LogEntry.LogEntry(Term=> CurrentTerm, Index=>LastApplied, Peyload=>Payload.EmptyPayload()),CommitIndex)));
--                    declare
--                       EntryToSend   : LogEntry.LogEntry   :=
--                         LogEntry.LogEntry'
--                           (Term    => CurrentTerm, Index => LastApplied,
--                            Peyload => Payload.EmptyPayload);
--                       MessageToSend : Message.AppendEntry :=
--                         Message.AppendEntry'
--                           (Term         => CurrentTerm, LeaderId => Id,
--                            PrevLogIndex => LastApplied,
--                            PrevLogTerm  => Log (LastApplied).Term,
--                            LogEntri     => EntryToSend,
--                            LeaderCommit => CommitIndex);
--                    begin
--                       Broadcast (Id => Id, Net => Net, Msg => MessageToSend);
--                    end;

--                    LastAppendEntryTimestamp := Clock;
--                 end if;
--              end;

--           end if;

--           --Follower election timeout managment
--           if (CurrentState = FOLLOWER) then

--              declare

--                 Milliseconds_From_Last_Heartbeat : Integer :=
--                   Integer (To_Duration (Clock - LastAppendEntryTimestamp)) *
--                   1_000;

--              begin

--                 if (Milliseconds_From_Last_Heartbeat > TimeoutDuration) then
--                    -- Election timeout expired
--                    -- Set state to CANDIDATE
--                    -- Update the term variable
--                    -- Update the AppendEntryTimeStamp
--                    CurrentState         := CANDIDATE;
--                    CurrentTerm          := CurrentTerm + 1;
--                    VotesCounter         := 1;
--                    CandidationTimestamp := Clock;
--                    Broadcast
--                      (Id  => Id, Net => Net,
--                       Msg =>
--                         Message.RequestVote'
--                           (Term         => CurrentTerm, CandidateId => Id,
--                            LastLogIndex => Log (Log.Last_Index).Index,
--                            LastLogTerm  => Log (Log.Last_Index).Term));
--                    Logger.Log
--                      (File_Name => LogFileName, Content => "Requested vote...");
--                 end if;

--              end;

--           end if;

--           if (CurrentState = CANDIDATE) then
--              --PRECONDITION: Election Timeout has expired so wait another timeout duration to request another election
--              --(c) a period of time goes by with no winner.
--              -- (c.1) Send another RequestVote

--              --  The third possible outcome is that a candidate neither
--              --  wins nor loses the election: if many followers become
--              --  candidates at the same time, votes could be split so that
--              --  no candidate obtains a majority. When this happens, each
--              --  candidate will time out and start a new election by incre-
--              --  menting its term and initiating another round of Request-
--              --  Vote RPCs.
--              declare
--                 TimeSpanFromCandidation : Integer :=
--                   Integer (To_Duration (Clock - CandidationTimestamp)) * 1_000;
--              begin
--                 if TimeSpanFromCandidation > TimeoutDuration then
--                    CurrentState         := CANDIDATE;
--                    CurrentTerm          := CurrentTerm + 1;
--                    VotesCounter         := 1;
--                    CandidationTimestamp := Clock;
--                    Broadcast
--                      (Id  => Id, Net => Net,
--                       Msg =>
--                         Message.RequestVote'
--                           (Term         => CurrentTerm, CandidateId => Id,
--                            LastLogIndex => Log (Log.Last_Index).Index,
--                            LastLogTerm  => Log (Log.Last_Index).Term));
--                    Logger.Log
--                      (File_Name => LogFileName, Content => "Requested vote...");
--                 end if;
--              end;
--           end if;
--           delay 0.3;

--        end loop;
--     end Node;

--     procedure Broadcast
--       (Id  : Integer; Net : access QueueVector.Vector;
--        Msg : Message.Message'Class)
--     is
--     begin
--        for I in Net.all.First_Index .. Net.all.Last_Index loop
--           if I /= Id then
--              Queue.Enqueue (net.all (I).all, Msg);
--           end if;
--        end loop;
--     end Broadcast;

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

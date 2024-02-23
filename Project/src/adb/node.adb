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

      --  Persistent state on all servers:
      --  (Updated on stable storage before responding to RPCs)

      -- latest term server has seen (initialized to 0 on first boot, increases monotonically)
      CurrentTerm : aliased Integer := 0;
      -- candidateId that received vote in current term (or null if none)
      VotedFor    : aliased Integer := -1;
      --log entries; each entry contains command for state machine, and term when entry was received by leader (first index is 1)
      Log         : aliased LogEntryVector.Vector;

      --  Volatile state on all servers:

      --  Index of highest log entry known to be committed (initialized to 0, increases monotonically)
      CommitIndex : aliased Integer;
      --  Index of highest log entry applied to state machine (initialized to 0, increases monotonically)
      LastApplied : aliased Integer;

      --  Volatile state on leaders:
      --  (Reinitialized after election)

      -- for each server, index of the next log entry to send to that server (initialized to leader last log index + 1)
      NextIndex  : aliased Integer;
      --  for each server, index of highest log entry known to be replicated on server (initialized to 0, increases monotonically)
      MatchIndex : aliased Integer;

      -- Cosa nostra
      TimeoutDuration                  : Integer;
      Milliseconds_From_Last_Heartbeat : Integer := 0;

      AppendedCounter : aliased Integer := 0;
      VotesCounter    : aliased Integer := 0;

      LastAppendEntryTimestamp : aliased Time := Clock;

      CurrentState : aliased State := FOLLOWER;

      --  Log
      LogFileName : constant String :=
        "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

      package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
      Gen : Integer_Random.Generator;

   begin
      --  Costants initialization
      Integer_Random.Reset (Gen);

      --  Variable initialization
      TimeoutDuration :=
        (Integer_Random.Random (Gen) mod 10 + 3) *
        1_000; -- TODO : Change to mod 151 + 150

      -- Loop
      loop

         while not Queue.Is_Empty (net.all (id).all) loop
            --  Start_Time := Clock;
            --  Net           : access QueueVector.Vector; Id : Integer;
            --  Msg           : Message.Message'Class; LastHeartbeat : access Time;
            --  CurrentLeader : access Integer; CurrentTerm : access Integer;
            --  CurrentState  : access State; Log : in out LogEntryVector.Vector;
            --  CommitIndex   : access Integer; VotedFor : access Integer
            HandleMessage
              (Net, Id, Queue.Dequeue (net.all (Id).all),
               LastAppendEntryTimestamp'Access, CurrentTerm'Access,
               CurrentState'Access, Log, CommitIndex'Access, VotedFor'Access,
               VotesCounter'Access);
            --  End_Time     := Clock;
            --  Milliseconds :=
            --    Integer (To_Duration (End_Time - Start_Time)) * 1_000;
         end loop;

         Milliseconds_From_Last_Heartbeat :=
           Integer (To_Duration (Clock - LastAppendEntryTimestamp)) * 1_000;

         --     --Managment Last_Heartbeat using milliseconds
         if (CurrentState = LEADER) then
            if (Integer (Milliseconds_From_Last_Heartbeat) > TimeoutDuration)
            then
            Put_Line ("Leader");
               -- Heartbeat expired
               -- Send heartbeat to all the nodes
               -- Make Last_Heartbeat = 0
               --Broadcast(Id, Net, Message.AppendEntry'(CurrentTerm,Id, LastApplied, Log(LastApplied).Term,
               --LogEntry.LogEntry(Term=> CurrentTerm, Index=>LastApplied, Peyload=>Payload.EmptyPayload()),CommitIndex)));
               declare
                  EntryToSend   : LogEntry.LogEntry   :=
                    LogEntry.LogEntry'
                      (Term    => CurrentTerm, Index => LastApplied,
                       Peyload => Payload.EmptyPayload);
                  MessageToSend : Message.AppendEntry :=
                    Message.AppendEntry'
                      (Term         => CurrentTerm, LeaderId => Id,
                       PrevLogIndex => LastApplied,
                       PrevLogTerm  => Log (LastApplied).Term,
                       LogEntri => EntryToSend, LeaderCommit => CommitIndex);
               begin
                  Put_Line ("Heartbeat");

                  Broadcast (Id => Id, Net => Net, Msg => MessageToSend);
               end;

               LastAppendEntryTimestamp := Clock;
            end if;
         end if;
         --Follower election timeout managment
         if (CurrentState = FOLLOWER) then
            if (Milliseconds_From_Last_Heartbeat > TimeoutDuration) then
               -- Election timeout expired
               -- Set state to CANDIDATE
               -- Update the term variable
               -- Update the AppendEntryTimeStamp
               CurrentState := CANDIDATE;
               CurrentTerm  := CurrentTerm + 1;
               Put_Line ("Cane di dio");
            end if;
         end if;
         if (CurrentState = CANDIDATE) then
            Logger.Log
              (File_Name => LogFileName, Content => "Became a Candidate...");

            --PRECONDITION: Election Timeout has expired so wait another timeout duration to request another election
            --(c) a period of time goes by with no winner.
            -- (c.1) Send another RequestVote
            declare
            begin
               Broadcast
                 (Id  => Id, Net => Net,
                  Msg =>
                    Message.RequestVote'
                      (Term         => CurrentTerm, CandidateId => Id,
                       LastLogIndex => Log (Log.Last_Index).Index,
                       LastLogTerm  => Log (Log.Last_Index).Term));
            exception
               when others =>
                  Broadcast
                    (Id  => Id, Net => Net,
                     Msg =>
                       Message.RequestVote'
                         (Term         => CurrentTerm, CandidateId => Id,
                          LastLogIndex => 0, LastLogTerm => 0));
            end;
            Logger.Log
              (File_Name => LogFileName, Content => "Requested vote...");
         end if;
         delay 0.3;
      end loop;
   end Node;

   procedure Broadcast
     (Id  : Integer; Net : access QueueVector.Vector;
      Msg : Message.Message'Class)
   is
   begin
      for I in Net.all.First_Index .. Net.all.Last_Index loop
         if I /= Id then
            Queue.Enqueue (net.all (I).all, Msg);
         end if;
      end loop;
   exception

      --  2. Reply false if log doesn’t contain an entry at prevLogIndex
      --     whose term matches prevLogTerm (§5.3)
      when Constraint_Error =>
         Put_Line ("Exception");
   end Broadcast;

   procedure SendToId
     (Net      : access QueueVector.Vector; Msg : Message.Message'Class;
      Receiver : Integer)
   is
   begin
      Queue.Enqueue (net.all (Receiver).all, Msg);

   end SendToId;

   procedure HandleMessage
     (Net         :        access QueueVector.Vector; Id : Integer;
      Msg :    Message.Message'Class; LastAppendEntryTimestamp : access Time;
      CurrentTerm :        access Integer; CurrentState : access State;
      Log         : in out LogEntryVector.Vector; CommitIndex : access Integer;
      VotedFor    :        access Integer; VotesCounter : access Integer)
   is
      LogFileName : constant String :=
        "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);
   begin

      case CurrentState.all is

         when FOLLOWER =>
            if Msg in AppendEntry'Class then

               LastAppendEntryTimestamp.all := Clock;

               --  1. Reply false if term < currentTerm (§5.1)
               --  2. Reply false if log doesn’t contain an entry at prevLogIndex
               --     whose term matches prevLogTerm (§5.3)
               --  3. If an existing entry conflicts with a new one (same index
               --     but different terms), delete the existing entry and all that
               --     follow it (§5.3)
               --  4. Append any new entries not already in the log
               --  5. If leaderCommit > commitIndex, set commitIndex =
               --     min(leaderCommit, index of last new entry)

               declare
                  MessageLogEntry          : LogEntry.LogEntry :=
                    AppendEntry (Msg).LogEntri;
                  MessageTerm              : Integer := AppendEntry (Msg).Term;
                  MessagePrevLogIndex      : Integer           :=
                    AppendEntry (Msg).PrevLogIndex;
                  MessageLeaderCommitIndex : Integer           :=
                    AppendEntry (Msg).LeaderCommit;
                  MessageLeaderId : Integer := AppendEntry (Msg).LeaderId;
               begin

                  --  1. Reply false if term < currentTerm (§5.1)
                  if MessageTerm < CurrentTerm.all then
                     SendToId
                       (Net      => Net,
                        Msg      =>
                          Message.AppendEntryResponse'
                            (Term => CurrentTerm.all, Success => False),
                        Receiver => MessageLeaderId);
                     return;
                  end if;

                  declare
                     Element : LogEntry.LogEntry;
                  begin
                     Element := Log (MessagePrevLogIndex);

                     --  3. If an existing entry conflicts with a new one (same index
                     --     but different terms), delete the existing entry and all that
                     --     follow it (§5.3)
                     if Element.Term /= MessageTerm then
                        Log.Delete
                          (Element.Index,
                           Log.Length -
                           Ada.Containers.Count_Type (Element.Index));
                     end if;

                     --  4. Append any new entries not already in the log
                     LogEntryVector.Append (Log, MessageLogEntry);

                     --  5. If leaderCommit > commitIndex, set commitIndex =
                     --     min(leaderCommit, index of last new entry)
                     CommitIndex.all :=
                       Integer'Min
                         (MessageLeaderCommitIndex, MessageLogEntry.Index);

                     SendToId
                       (Net      => Net,
                        Msg      =>
                          Message.AppendEntryResponse'
                            (Term => CurrentTerm.all, Success => True),
                        Receiver => MessageLeaderId);

                  exception

                     --  2. Reply false if log doesn’t contain an entry at prevLogIndex
                     --     whose term matches prevLogTerm (§5.3)
                     when Constraint_Error =>

                        SendToId
                          (Net      => Net,
                           Msg      =>
                             Message.AppendEntryResponse'
                               (Term => CurrentTerm.all, Success => False),
                           Receiver => MessageLeaderId);
                        return;

                  end;

               end;

            elsif Msg in RequestVote'Class then

               --  1. Reply false if term < currentTerm (§5.1)
               --  2. If votedFor is null or candidateId, and candidate’s log is at
               --     least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)

               declare

                  MessageTerm         : Integer := RequestVote (Msg).Term;
                  MessageCandidateId  : Integer :=
                    RequestVote (Msg).CandidateId;
                  MessageLastLogIndex : Integer :=
                    RequestVote (Msg).LastLogIndex;
                  MessageLastLogTerm  : Integer :=
                    RequestVote (Msg).LastLogTerm;

               begin
                  Logger.Log
                    (File_Name => LogFileName,
                     Content   =>
                       "Request vote from " &
                       Integer'Image (MessageCandidateId));

                  --  1. Reply false if term < currentTerm (§5.1)
                  if MessageTerm < CurrentTerm.all then
                     SendToId
                       (Net      => Net,
                        Msg      =>
                          Message.RequestVoteResponse'
                            (Term => CurrentTerm.all, VoteGranted => False),
                        Receiver => MessageCandidateId);
                     return;
                  end if;

                  declare
                     LastLogElement : LogEntry.LogEntry;
                  begin
                     LastLogElement := Log (Log.Last_Index);

                     --  2. If votedFor is null or candidateId, and candidate’s log is at
                     --     least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)
                     if
                       (VotedFor.all = -1 or
                        VotedFor.all = MessageCandidateId) and
                       (LastLogElement.Index <= MessageLastLogIndex and
                        LastLogElement.Term <= MessageLastLogTerm)
                     then
                        SendToId
                          (Net      => Net,
                           Msg      =>
                             Message.RequestVoteResponse'
                               (Term => CurrentTerm.all, VoteGranted => True),
                           Receiver => MessageCandidateId);

                        VotedFor.all := MessageCandidateId;

                        Logger.Log
                          (File_Name => LogFileName,
                           Content   =>
                             "Vote granted to " &
                             Integer'Image (MessageCandidateId));
                        return;

                     else
                        SendToId
                          (Net      => Net,
                           Msg      =>
                             Message.RequestVoteResponse'
                               (Term => CurrentTerm.all, VoteGranted => False),
                           Receiver => MessageCandidateId);
                     end if;
                  exception
                     when Constraint_Error =>
                        -- Handle the case where the index is out of bounds
                        SendToId
                          (Net      => Net,
                           Msg      =>
                             Message.RequestVoteResponse'
                               (Term => CurrentTerm.all, VoteGranted => True),
                           Receiver => MessageCandidateId);

                        VotedFor.all := MessageCandidateId;

                        Logger.Log
                          (File_Name => LogFileName,
                           Content   =>
                             "Vote granted to " &
                             Integer'Image (MessageCandidateId));

                        return;
                  end;
               end;

            else
               --  TODO : Theoretically impossible
               null;
            end if;

         when CANDIDATE =>
            if Msg in AppendEntry'Class then
               --  TODO
               null;
            elsif Msg in RequestVoteResponse'Class then
               declare
                  MessageSuccess : Boolean :=
                    RequestVoteResponse (Msg).VoteGranted;
                  MessageTerm    : Integer := RequestVoteResponse (Msg).Term;

                  NetLenght : Integer := Integer (Net.all.Length);
               begin
                  Logger.Log
                    (File_Name => LogFileName,
                     Content   =>
                       "Voted received:" & Boolean'Image (MessageSuccess));
                  if MessageSuccess then
                     VotesCounter.all := VotesCounter.all + 1;

                     if VotesCounter.all > Integer (NetLenght / 2) then
                        CurrentState.all := LEADER;

                        Logger.Log
                          (File_Name => LogFileName,
                           Content   => "Now LEADER...");
                     end if;

                  end if;
               end;
            else
               --  TODO : Theoretically impossible
               null;
            end if;

         when LEADER =>
            if Msg in AppendEntryResponse'Class then
               --  TODO
               null;
            elsif Msg in AppendEntry'Class then
               --  TODO
               null;
            else
               --  TODO : Theoretically impossible
               null;
            end if;
      end case;
   end HandleMessage;

end Node;

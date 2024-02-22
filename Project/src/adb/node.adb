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
      CurrentTerm : Integer := 0;
      -- candidateId that received vote in current term (or null if none)
      VotedFor    : Integer := -1;
      --log entries; each entry contains command for state machine, and term when entry was received by leader (first index is 1)
      Log         : aliased LogEntryVector.Vector;

      --  Volatile state on all servers:

      --  Index of highest log entry known to be committed (initialized to 0, increases monotonically)
      CommitIndex : Integer;
      --  Index of highest log entry applied to state machine (initialized to 0, increases monotonically)
      LastApplied : Integer;

      --  Volatile state on leaders:
      --  (Reinitialized after election)

      -- for each server, index of the next log entry to send to that server (initialized to leader last log index + 1)
      NextIndex  : Integer;
      --  for each server, index of highest log entry known to be replicated on server (initialized to 0, increases monotonically)
      MatchIndex : Integer;

      -- Cosa nostra
      TimoutDuration : Integer;
      Milliseconds_From_Last_Heartbeat : Integer:=0;

      AppendedCounter : Integer := 0;
      VotesCounter    : Integer := 0;

      LastAppendEntryTimestamp : aliased Time := Clock;
      Current_Time             : Time         := Clock;

      Current_State : aliased State := FOLLOWER;

      --  Log
      LogFileName : constant String :=
        "Node_" & Trim (Integer'Image (Id), Ada.Strings.Left);

      package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);
      Gen : Integer_Random.Generator;

   begin
      --  Costants initialization
      Integer_Random.Reset (Gen);

      --  Variable initialization
      TimoutDuration :=
        Integer_Random.Random (Gen) mod 151 +
        1_500; -- TODO : Change to mod 151 + 150
        Milliseconds_From_Last_Heartbeat:= Integer(To_Duration(LastAppendEntryTimestamp-Current_Time));

      -- Loop
      loop
       --     --Managment Last_Heartbeat using milliseconds
              if (Current_State = LEADER) then
                 if (Integer (Milliseconds_From_Last_Heartbeat) > TimoutDuration) then
                  -- Heartbeat expired
                  -- Send heartbeat to all the nodes
                  -- Make Last_Heartbeat = 0
                    --Broadcast(Id, Net, Message.AppendEntry'(CurrentTerm,Id, LastApplied, Log(LastApplied).Term, 
                      --LogEntry.LogEntry(Term=> CurrentTerm, Index=>LastApplied, Peyload=>Payload.EmptyPayload()),CommitIndex)));
                     Broadcast(Id => Id, Net => Net, Msg => Message.AppendEntry(CurrentTerm,Id, LastApplied, Log(LastApplied).Term, 
                      LogEntry.LogEntry(Term=> CurrentTerm, Index=>LastApplied, Peyload=>Payload.EmptyPayload()),CommitIndex));
                  
                    LastAppendEntryTimestamp:= Clock;
                  end if;
               end if;
              --Follower election timeout managment
               if (Current_State = FOLLOWER) then
                 if
                   (Integer (Milliseconds_From_Last_Heartbeat) > TimoutDuration)
                 then
                    -- Election timeout expired
                    -- Set state to CANDIDATE
                    -- Update the term variable
                    -- Update the AppendEntryTimeStamp
                    Current_State := CANDIDATE;
                    CurrentTerm  := CurrentTerm + 1;
                    LastAppendEntryTimestamp:= Clock;
                 end if;
               end if;
               if(Current_State = CANDIDATE) then
                  --PRECONDITION: Election Timeout has expired so wait another timeout duration to request another election
                  if(Integer(Milliseconds_From_Last_Heartbeat)>TimoutDuration) then
                     --(c) a period of time goes by with no winner.
                     -- (c.1) Send another RequestVote
                     Broadcast(Id => Id, Net => Net, Msg => Message.RequestVote'(Term=> CurrentTerm, CandidateId=> Id, LastLogIndex=>Log(Log.Last_Index).Index, LastLogTerm=>Log(Log.Last_Index).Term));
                  end if;
               end if;
         --  CANDIDATE TimeoutDuration managment
         --  A candidate continues in
         --  this state until one of three things happens: (a) it wins the
         --  election, (b) another server establishes itself as leader, or
         --  (c) a period of time goes by with no winner.

         --  A candidate wins an election if it receives votes from
         --  a majority of the servers in the full cluster for the same
         --  term. Each server will vote for at most one candidate in a
         --  given term, on a first-come-first-served basis (note: Sec-
         --  tion 5.4 adds an additional restriction on votes). The ma-
         --  jority rule ensures that at most one candidate can win the
         --  election for a particular term (the Election Safety Prop-
         --  erty in Figure 3). Once a candidate wins an election, it
         --  becomes leader. It then sends heartbeat messages to all of
         --  the other servers to establish its authority and prevent new
         --  elections.

         --  While waiting for votes, a candidate may receive an
         --  AppendEntries RPC from another server claiming to be
         --  leader. If the leader’s term (included in its RPC) is at least
         --  as large as the candidate’s current term, then the candidate
         --  recognizes the leader as legitimate and returns to follower
         --  state. If the term in the RPC is smaller than the candidate’s
         --  current term, then the candidate rejects the RPC and con-
         --  tinues in candidate state.

         --  The third possible outcome is that a candidate neither
         --  wins nor loses the election: if many followers become
         --  candidates at the same time, votes could be split so that
         --  no candidate obtains a majority. When this happens, each
         --  candidate will time out and start a new election by incre-
         --  menting its term and initiating another round of Request-
         --  Vote RPCs. However, without extra measures split votes
         --  could repeat indefinitely.

         Logger.Log (File_Name => LogFileName, Content => "Node Running...");

         --  declare
         --     Start_Time, End_Time : Time;
         --     Milliseconds         : Integer := 0;
         --  begin
         --     --Managment Last_Heartbeat using milliseconds
         --     if (Current_State = LEADER) then
         --        if
         --          (Integer (Milliseconds_From_Last_Heartbeat) >
         --           Heartbeat_Timeout_Duration + Milliseconds)
         --        then
         --           -- Heartbeat expired
         --           -- Send heartbeat to all the nodes
         --           -- Make Last_Heartbeat = 0
         --           Broadcast
         --             (Id, Net,
         --              Message.Heartbeat'
         --                (Sender_Id  => Id, Term => Current_Term,
         --                 Log_length => Integer (Log.Length)));
         --           Last_Heartbeat := Clock;
         --        end if;
         --     end if;
         --     --Follower election timeout managment
         --     if (Current_State = FOLLOWER) then
         --        if
         --          (Integer (Milliseconds_From_Last_Heartbeat) >
         --           Election_Timeout_Duration + Milliseconds)
         --        then
         --           -- Election timeout expired
         --           -- Set state to CANDIDATE
         --           -- Update votes counter
         --           -- Update the term variable
         --           -- Send candidation to all the nodes
         --           -- Make Last_Heartbeat = 0
         --           Current_State := CANDIDATE;
         --           Current_Term  := Current_Term + 1;
         --           Votes_Counter := Votes_Counter + 1;
         --           Broadcast
         --             (Id, Net,
         --              Message.Candidated'
         --                (Sender_Id  => Id, Term => Current_Term,
         --                 Log_length => Integer (Log.Length)));
         --           Last_Heartbeat := Clock;
         --        end if;
         --     end if;

         --     if not Queue.Is_Empty (net.all (id).all) then
         --        Start_Time := Clock;
         --        HandleMessage
         --          (Net, Id, Queue.Dequeue (net.all (Id).all),
         --           Last_Heartbeat'Access, Current_Leader'Access,
         --           Current_Term'Access, Current_State'Access, Log,
         --           Votes_Counter);
         --        End_Time     := Clock;
         --        Milliseconds :=
         --          Integer (To_Duration (End_Time - Start_Time)) * 1_000;
         --     end if;

         --  end;
         delay 1.0;
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
   end Broadcast;

   procedure SendToLeader
     (Current_Leader : Integer; Net : access QueueVector.Vector;
      Msg            : Message.Message'Class)
   is
   begin
      Put_Line ("Boiadioasidoki");
   end SendToLeader;

   procedure SendToId
     (Net      : access QueueVector.Vector; Msg : Message.Message'Class;
      Receiver : Integer)
   is
   begin
      Queue.Enqueue (net.all (Receiver).all, Msg);

   end SendToId;

   procedure HandleMessage
     (Net           : access QueueVector.Vector; Id : Integer;
      Msg           : Message.Message'Class; LastHeartbeat : access Time;
      CurrentLeader : access Integer; CurrentTerm : access Integer;
      CurrentState  : access State; Log : in out LogEntryVector.Vector;
      CommitIndex   : access Integer; VotedFor : access Integer)
   is
   begin
      case CurrentState.all is

         when FOLLOWER =>
            if Msg in AppendEntry'Class then

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
                        return;
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
               --  TODO
               null;
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

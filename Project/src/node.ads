package Node is

   type State is (LEADER, CANDIDATE, FOLLOWER);

    type Node;
   type NodeAccess is access all Node;
   type NodeArray is array (Positive range <>) of NodeAccess;
   type NodeArrayAccess is access all NodeArray;

   NodeAccessArray : aliased NodeArray := NodeArray'(1 .. 3 => null);

   task type Node (id : Integer; net : NodeArrayAccess) is
      entry Send_Message (Message_Type : in MessageType; Msg : in String);
   end Node;

end Node;

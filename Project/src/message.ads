package Message_Package is


    type Message is tagged record
        Sender_Id : Integer;
    end record;
    type Heartbeat is new Message;
    type Commited is new Message;
    type Commit is new Message;
    type Appended is new Message;
    type Candidated is new Message;
    type Vote is new Message;
    type AppendEntry is new Message;

end Message_Package;

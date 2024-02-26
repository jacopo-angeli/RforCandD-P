with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Message;

package body Queue is
    
    -- Adds a message to the end of the queue.
    -- @param Q The queue to which the message will be added.
    -- @param Msg The message object to be enqueued (added).
    procedure Enqueue (Q : in out Queue; Msg : in Message.Message'Class) is
    begin
        -- Appends a new instance of the message to the vector 'Data' in 'Q'.
        Vector_Ptrs.Append (Q.Data, new Message.Message'Class'(Msg));
    end Enqueue;

    -- Removes and returns the first message from the queue.
    -- @param Q The queue from which the message will be dequeued (removed).
    -- @return The message object at the front of the queue.
    -- @throws Constraint_Error if the queue is empty.
    function Dequeue (Q : in out Queue) return Message.Message'Class is
        -- Temporary storage for the message to be dequeued.
        Msg : Message.Message'Class := Q.Data.First_Element.all;
    begin
        -- Checks if the queue is not empty before attempting to dequeue.
        if not Is_Empty (Q) then
            -- Deletes the first element from the vector 'Data' in 'Q'.
            Vector_Ptrs.Delete (Q.Data, Q.Data.First_Index);
        else
            -- Raises an error if an attempt is made to dequeue from an empty queue.
            raise Constraint_Error with "Queue is empty";
        end if;
        return Msg;
    end Dequeue;

    -- Checks if the queue is empty.
    -- @param Q The queue to be checked.
    -- @return True if the queue is empty, otherwise False.
    function Is_Empty (Q : Queue) return Boolean is
    begin
        -- Returns True if the 'Data' vector in 'Q' is empty.
        return Q.Data.Is_Empty;
    end Is_Empty;

    -- Returns the number of messages in the queue.
    -- @param Q The queue whose length is to be determined.
    -- @return The number of items in the queue.
    function Length(Q: in Queue) return Ada.Containers.Count_Type is
    begin
        -- Returns the length of the vector 'Data' in 'Q'.
        return Vector_Ptrs.Length(Q.Data);
    end Length;

    procedure Clear(Q: in out Queue)is
    begin
        Vector_Ptrs.Clear(Q.data);
    end;

end Queue;

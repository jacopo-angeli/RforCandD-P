with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Message;

package body Queue is

    procedure Initialize (Q : out Queue) is
    begin
        Q.Data := Vector_Ptrs.Empty_Vector;
    end Initialize;

    procedure Enqueue (Q : in out Queue; Msg : in Message.Message'Class) is
    begin
        Vector_Ptrs.Append (Q.Data, new Message.Message'Class'(Msg));
    end Enqueue;

    function Dequeue (Q : in out Queue) return Message.Message'Class is
        Msg : Message.Message'Class := Q.Data.First_Element.all;
    begin
        if not Is_Empty (Q) then
            Vector_Ptrs.Delete (Q.Data, Q.Data.First_Index);
        else
            raise Constraint_Error with "Queue is empty";
        end if;
        return Msg;
    end Dequeue;

    function Is_Empty (Q : Queue) return Boolean is
    begin
        return Q.Data.Is_Empty;
    end Is_Empty;

    function Lenght(Q: in Queue) return Ada.Containers.Count_Type is
    begin
        return Vector_Ptrs.Length(Q.Data);
    end Lenght;

end Queue;

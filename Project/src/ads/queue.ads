with Ada.Containers.Vectors;
with Message;

package Queue is

    type MessagePtr is access all Message.Message'Class;

    package Vector_Ptrs is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => MessagePtr);

    type Queue is record
        Data  : Vector_Ptrs.Vector;
        Rear  : Natural := 1;
    end record;

    procedure Enqueue (Q : in out Queue; Msg : in Message.Message'Class);
    function Dequeue (Q : in out Queue) return Message.Message'Class;
    function Length (Q: in Queue) return Ada.Containers.Count_Type;
    function Is_Empty (Q : Queue) return Boolean;

end Queue;

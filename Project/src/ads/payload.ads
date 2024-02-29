with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
package Payload is

     use Ada.Strings.Unbounded;

    type PayloadType is
       (MONEYTRANSFER, ACCOUNTOPEN, WITHDRAWAL, DEPOSIT, EMPTY);
    --  """INTERFACE"""
    type Payload is record

        Sort         : PayloadType;
        From_Account : Unbounded_String;
        To_Account   : Unbounded_String;
        Quantity     : Float;
        New_Account  : Unbounded_String;
        Owner        : Unbounded_String;

    end record;

    function EmptyPayload return Payload;
    function "=" (Left, Right : Payload) return Boolean;
    package PayloadVector is new Ada.Containers.Vectors
       (Index_Type   => Natural,--
        Element_Type => Payload,--
        "="          => "=");


end Payload;

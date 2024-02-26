with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Payload is

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
end Payload;

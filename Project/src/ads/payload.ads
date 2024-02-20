package Payload is
    --  """INTERFACE"""
    type Payload is tagged null record;

    --  SUBTYPES
    type MoneyTransfer is new Payload with record
        From_Account : String (1 .. 255);
        To_Account   : String (1 .. 255);
        Quantity     : Float;
    end record;

    type AccountOpen is new Payload with record
        New_Account : String (1 .. 255);
        Owner       : String (1 .. 255);
    end record;

    type Withdrawal is new Payload with record
        From_Account : String (1 .. 255);
        Quantity     : Float;
    end record;

    type Deposit is new Payload with record
        To_Account : String (1 .. 255);
        Quantity   : Float;
    end record;

end Payload;

with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

package body Payload is

    function RandomPayload return Payload is

        package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);

        FloatGen   : Ada.Numerics.Float_Random.Generator;
        IntegerGen : Integer_Random.Generator;

        Frequency : Float; --Hz
        Amplitude : Float; --Meters
        Duration  : Integer; --Fake time span
        Magnitudo : Integer;
        Depth     : Float;

    begin
        Ada.Numerics.Float_Random.Reset (FloatGen);
        Integer_Random.Reset (IntegerGen);

        Frequency := Ada.Numerics.Float_Random.Random (FloatGen);
        Amplitude := Ada.Numerics.Float_Random.Random (FloatGen);
        Duration  := Integer_Random.Random (IntegerGen);
        Magnitudo := Integer_Random.Random (IntegerGen);
        Depth     := Ada.Numerics.Float_Random.Random (FloatGen);

        return Payload'
              (Frequency,--
               Amplitude,--
               Duration,--
               Magnitudo,--
               Depth);
    end RandomPayload;

end Payload;

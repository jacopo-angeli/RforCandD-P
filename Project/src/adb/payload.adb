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

        Frequency := ABS(Ada.Numerics.Float_Random.Random (FloatGen));
        Amplitude := ABS(Ada.Numerics.Float_Random.Random (FloatGen));
        Duration  := ABS(Integer_Random.Random (IntegerGen));
        Magnitudo := ABS(Integer_Random.Random (IntegerGen) mod 8 + 1);
        Depth     := ABS(Ada.Numerics.Float_Random.Random (FloatGen));

        return Payload'
              (Frequency,--
               Amplitude,--
               Duration,--
               Magnitudo,--
               Depth);
    end RandomPayload;
    function Payload_Stringify (obj : in Payload) return String is
    begin
        return
           "{ Frequency: " & Float'Image (obj.Frequency) & ", Amplitude: " &
           Float'Image (obj.Amplitude) & ", Duration: " &  Integer'Image(obj.Duration) & 
           ", Magnitudo: " & Integer'Image(obj.Magnitudo) & 
           ", Depth: " & Float'Image(obj.Depth) & "}";
    end Payload_Stringify;

end Payload;

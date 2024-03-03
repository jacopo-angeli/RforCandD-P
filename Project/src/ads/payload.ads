with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
package Payload is

   use Ada.Strings.Unbounded;

   --  """INTERFACE"""
   type Payload is record

      Frequency : Float; --Hz
      Amplitude : Float; --Meters
      Duration  : Integer; --Fake time span
      Magnitudo : Integer;
      Depth     : Float;
   end record;
   function CreatePayload(Frequency : Float; Amplitude: Float; Duration : Integer; Magnitudo : Integer; Depth : Float) return Payload;
   package PayloadVector is new Ada.Containers.Vectors
     (Index_Type   => Natural,--
      Element_Type => Payload,--
      "="          => "=");

end Payload;

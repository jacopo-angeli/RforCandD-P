with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
package body Payload is
function CreatePayload(Frequency : Float; Amplitude: Float; Duration : Integer; Magnitudo : Integer; Depth : Float) return Payload is
Payloadz : Payload;
begin
    Payloadz.Frequency:=Frequency;
    Payloadz.Amplitude:=Amplitude;
    Payloadz.Duration:=Duration;
    Payloadz.Magnitudo:=Magnitudo;
    Payloadz.Depth:=Depth;
return Payloadz;
end CreatePayload;

end Payload;
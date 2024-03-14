with Node;
with LogEntry; use LogEntry;
with Payload;
package Logger is

   procedure Log (File_Name: String; Content: String);

   procedure DB(File_Name: String; Content:  Payload.PayloadVector.Vector);

end Logger;

with Node;
with LogEntry; use LogEntry;
package Logger is

   procedure Log (File_Name: String; Content: String);

   procedure PrettyPrint(File_Name: String; Content:  LogEntryVector.Vector);

end Logger;

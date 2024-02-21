with Node;
package Logger is

   type Table_Row is record
      Id             : Integer;
      Current_Leader : Integer;
      Current_Term   : Integer;
      Current_State  : Node.State;
      Vector_Entries : Node.LogEntryVector.Vector;
   end record;

   procedure Log_Row (File_Name: String; Row : Table_Row);

end Logger;

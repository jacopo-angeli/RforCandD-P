with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with LogEntry;

package body Logger is

    procedure Log_Row (File_Name : String; Row : Table_Row) is
        File : File_Type;
    begin
        begin
            Open
               (File => File, Mode => Append_File,
                Name => ("logs/" & File_Name & ".log"));
            -- If the file opens successfully, it exists.
        exception
            when E : others =>
                -- If an exception occurs, assume the file does not exist.
                Create
                   (File => File, Mode => Append_File,
                    Name => ("logs/" & File_Name & ".log"));
        end;
        --  Time Stamp
        Put (File => File, Item => Image (Clock));

        --  Current leader
        Put (File => File,
            Item  =>
               " $ Current leader : " & Integer'Image (Row.Current_Leader));

        --  Current state
        Put (File => File, Item => ", Current state : ");
        case Row.Current_State is
            when Node.LEADER =>
                Put (File => File, Item => "LEADER");
            when Node.CANDIDATE =>
                Put (File => File, Item => "CANDIDATE");
            when Node.FOLLOWER =>
                Put (File => File, Item => "FOLLOWER");
        end case;

        -- Current term
        Put (File => File,
          Item    => ", Current term : " & Integer'Image (Row.Current_Term));

        -- Last log entry
        declare
            Content : String := "empty";
        begin
            if not Row.Vector_Entries.Is_Empty then
                Content :=
                   LogEntry.Entry_Stringify
                      (Row.Vector_Entries (Row.Vector_Entries.Last_Index));
            end if;
            Put_Line (File => File, Item => ", Last log entry : " & Content & " ;");
        end;

        Close (File);
    end Log_Row;

end Logger;

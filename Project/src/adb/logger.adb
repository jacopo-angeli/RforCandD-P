with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with LogEntry;

package body Logger is

    procedure Log (File_Name : String; Content : String) is
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
        Put (File => File, Item => Image (Clock) & " $ ");
        Put_Line (File => File, Item => Content);

        Close (File);
    end Log;

end Logger;

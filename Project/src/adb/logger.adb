with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with LogEntry;
with Ada.Strings.Unbounded;
with Config;

package body Logger is

    use Config;
    use Ada.Text_IO;
    use Ada.Strings.Fixed;
    use Ada.Calendar;
    use Ada.Calendar.Formatting;

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

    procedure DB (File_Name : String; Content : Payload.PayloadVector.Vector)
    is
        File : File_Type;
    begin
        begin
            Open
               (File => File, --
                Mode => DBLogFileType,--
                Name => ("logs/" & File_Name & ".log"));
            -- If the file opens successfully, it exists.
        exception
            when E : others =>
                -- If an exception occurs, assume the file does not exist.
                Create
                   (File => File, --
                    Mode => DBLogFileType, --
                    Name => ("logs/" & File_Name & ".log"));
        end;
        Put_Line (File => File, Item => Image (Clock) & " $ Log Content:");
        if Content.Is_Empty then
            Put_Line (File => File, Item => "Empty");
        else
            for I in Content.First_Index .. Content.Last_Index loop
                Put_Line
                   (File => File,
                    Item => (Payload.Payload_Stringify (Content (I))));
            end loop;
        end if;
        Put_Line (File, "");
        Close (File);
    end DB;

end Logger;

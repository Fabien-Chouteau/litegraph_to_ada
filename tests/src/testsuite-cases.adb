with Ada.Exceptions;
with Ada.Directories;
with Ada.Text_IO;

with AAA.Strings;

with Testsuite.Manager;
with Nodes;

package body Testsuite.Cases is

   package Manager is new Testsuite.Manager (Number_Of_Links => 100,
                                             Number_Of_Types => 100,
                                             Number_Of_Nodes => 100);

   type Required_Files is (Config, Expected, Input);

   function Filename (F : Required_Files) return String
   is (case F is
          when Config   => "config.txt",
          when Expected => "expected.txt",
          when Input    => "input.txt");

   ----------------
   -- Find_Cases --
   ----------------

   function Find_Cases return Test_Cases_List.List is
      use Ada.Directories;

      Result : Test_Cases_List.List;

      procedure Rec_Find (Path : String) is
         Found : array (Required_Files) of Boolean := (others => False);

         procedure Print (Item : Directory_Entry_Type) is
         begin
            for F in Required_Files loop
               if Simple_Name (Item) = Filename (F) then
                  Found (F) := True;
               end if;
            end loop;
         end Print;

         procedure Walk (Item : Directory_Entry_Type) is
         begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
            then
               Rec_Find (Path & "/" & Simple_Name (Item));
            end if;
         exception
            when Name_Error => null;
         end Walk;
      begin
         Search (Path, "*", (others => True), Print'Access);
         Search (Path, "", (Directory => True, others => False),
                 Walk'Access);

         if (for all F of Found => F) then
            --  We got all required files for a test case
            Result.Append (Path);

         elsif (for some F of Found => F) then
            --  At least one missing file
            for F in Required_Files loop
               if not Found (F) then
                  Ada.Text_IO.Put_Line ("warning: missing '" & Filename (F) &
                                          "' in '" & Path &
                                          "' for a complete test case.");
               end if;
            end loop;
         end if;
      end Rec_Find;
   begin
      Rec_Find ("test_cases");
      return Result;
   end Find_Cases;

   ------------------
   -- Load_Content --
   ------------------

   function Load_Content (Dir : String; F : Required_Files)
                          return AAA.Strings.Vector
   is
      use Ada.Text_IO;
      Result : AAA.Strings.Vector;
      File : File_Type;
   begin
      Open (File, In_File, Dir & "/" & Filename (F));
      while not End_Of_File (File) loop
         Result.Append (Get_Line (File));
      end loop;
      Close (File);
      return Result;
   end Load_Content;

   --------------
   -- Run_Case --
   --------------

   procedure Run_Case (Dirname : String) is
      use AAA.Strings;

      Config   : constant Vector := Load_Content (Dirname, Cases.Config);
      Input    : constant Vector := Load_Content (Dirname, Cases.Input);
      Expected : constant Vector := Load_Content (Dirname, Cases.Expected);

   begin
      Manager.Bounded_Manager.Reset;

      --  Manager.Bounded_Manager.Print_LG_Definitions;
      --  Ada.Text_IO.Put_Line (Config.Flatten (ASCII.LF));
      --  Ada.Text_IO.Put_Line (Input.Flatten (ASCII.LF));
      --  Ada.Text_IO.Put_Line (Expected.Flatten (ASCII.LF));

      for Line of Config loop
         Manager.Bounded_Manager.Load_Config_Line (Line);
      end loop;

      for Line of Input loop
         if Has_Prefix (Line, "input_a:") then
            Manager.Register_Input_A_Node.Singleton.Push
              ((Val => Integer'Value (Tail (Line, ':'))));
         elsif Has_Prefix (Line, "input_b:") then
            Manager.Register_Input_B_Node.Singleton.Push
              ((Val => Integer'Value (Tail (Line, ':'))));
         elsif Has_Prefix (Line, "input_c:") then
            Manager.Register_Input_C_Node.Singleton.Push
              ((Val => Integer'Value (Tail (Line, ':'))));
         end if;
      end loop;

      declare
         Output : constant AAA.Strings.Vector := Nodes.Get_Prints;
      begin
         if Output  = Expected then
            Ada.Text_IO.Put_Line ("PASS");
         else
            Ada.Text_IO.Put_Line ("FAIL");
            Ada.Text_IO.Put_Line
              (Diff (A           => Expected,
                     B           => Output,
                     A_Name      => "expected",
                     B_Name      => "output").Flatten (ASCII.LF));
         end if;
      end;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("FAIL (exception)");
         if True then
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         end if;
   end Run_Case;

   ---------------
   -- Run_Cases --
   ---------------

   procedure Run_Cases (List : Test_Cases_List.List) is
      Count : Positive := 1;
   begin
      for Dirname of List loop
         Ada.Text_IO.Put ("[" & Count'Img & "/" & List.Length'Img & "] " &
                            Dirname & " -> ");
         Run_Case (Dirname);
         Count := Count + 1;
      end loop;
   end Run_Cases;

end Testsuite.Cases;

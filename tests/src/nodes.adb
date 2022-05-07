package body Nodes is

   Lines : AAA.Strings.Vector;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Str : String) is
   begin
      Lines.Append (Str);
   end Print_Line;

   ----------------
   -- Get_Prints --
   ----------------

   function Get_Prints return AAA.Strings.Vector is
   begin
      return Lines;
   end Get_Prints;

   ------------------
   -- Reset_Prints --
   ------------------

   procedure Reset_Prints is
   begin
      Lines.Clear;
   end Reset_Prints;

end Nodes;

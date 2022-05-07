package body Nodes.Print_Prop is

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (This : in out Node; Key : String; Val : Integer)
   is
   begin
      Nodes.Print_Line ("Got property: '" & Key & "'='" & Val'Img & "'");
   end Set_Property;

end Nodes.Print_Prop;

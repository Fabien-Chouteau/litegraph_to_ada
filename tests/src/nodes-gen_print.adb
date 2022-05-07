package body Nodes.Gen_Print is

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive (This : in out Node; Port : Port_Id; Data : Link_Data)
   is
   begin
      Print_Line (This.Name & " got:" & Data.Val'Img);
   end Receive;

end Nodes.Gen_Print;

package body Nodes.Split is

   ------------------
   -- Set_Property --
   ------------------

   overriding
   procedure Set_Property (This : in out Node; Key : String; Val : Integer)
   is
   begin
      if Key = "split_point" then
         This.Split_Point := Val;
      end if;
   end Set_Property;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive (This : in out Node; Port : Port_Id; Data : Link_Data)
   is
   begin
      if Data.Val >= This.Split_Point then
         This.Send (0, Data);
      else
         This.Send (1, Data);
      end if;
   end Receive;

end Nodes.Split;

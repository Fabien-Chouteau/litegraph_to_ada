package body Nodes.Split is

   ------------------
   -- Set_Property --
   ------------------

   overriding
   procedure Set_Property (This : in out Node;
                           Id   :        Property_Id;
                           Val  :        Property_Value)
   is
   begin
      case Id is
         when 0 =>
            This.Split_Point := Val.Int_Val;
         when others =>
            null;
      end case;
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

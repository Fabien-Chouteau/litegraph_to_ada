package body Nodes.Gen_Bridge is

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive (This : in out Node; Port : Port_Id; Data : Link_Data)
   is
   begin
      This.Send (0, Data);
   end Receive;

end Nodes.Gen_Bridge;

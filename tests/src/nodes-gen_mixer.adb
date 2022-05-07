package body Nodes.Gen_Mixer is

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive (This : in out Node; Port : Port_Id; Data : Link_Data)
   is
   begin
      This.Send (1, Data);
   end Receive;

end Nodes.Gen_Mixer;

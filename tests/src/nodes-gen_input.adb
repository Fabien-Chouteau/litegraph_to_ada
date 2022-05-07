package body Nodes.Gen_Input is

   ----------
   -- Push --
   ----------

   procedure Push (This : Node; Data : Link_Data) is
   begin
      This.Send (0, Data);
   end Push;

end Nodes.Gen_Input;

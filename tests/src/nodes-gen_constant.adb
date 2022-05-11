package body Nodes.Gen_Constant is

   --------------
   -- On_Start --
   --------------

   overriding
   procedure On_Start (This : in out Node) is
   begin
      This.Send (0, Value);
   end On_Start;

end Nodes.Gen_Constant;

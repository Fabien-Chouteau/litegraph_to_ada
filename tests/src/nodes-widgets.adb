package body Nodes.Widgets is

   ------------------
   -- Set_Property --
   ------------------

   overriding
   procedure Set_Property (This : in out Node;
                           Id   :        Property_Id;
                           Val  :        Property_Value)
   is
   begin
      case Val.Kind is
         when Int_Prop =>
            Nodes.Print_Line ("Got int property: " & Val.Int_Val'Img);
         when Str_Prop =>
            Nodes.Print_Line ("Got str property: '" & Val.Str_Val & "'");
         when Bool_Prop =>
            Nodes.Print_Line ("Got bool property: " &
                              (if Val.Bool_Val then "True" else "False")
                              & "'");
      end case;
   end Set_Property;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This : in out Node; Port : Port_Id; Data : Link_Data)
   is
   begin
      This.Send (0, Data);
   end Receive;

end Nodes.Widgets;

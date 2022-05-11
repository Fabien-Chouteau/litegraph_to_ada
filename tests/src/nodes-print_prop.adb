package body Nodes.Print_Prop is

   ------------------
   -- Set_Property --
   ------------------

   overriding
   procedure Set_Property (This : in out Node;
                           Key  :        String;
                           Val  :        Property_Value)
   is
   begin
      case Val.Kind is
         when Int_Prop =>
            Nodes.Print_Line ("Got int property: '" & Key & "'='" &
                                Val.Int_Val'Img & "'");
         when Str_Prop =>
            Nodes.Print_Line ("Got str property: '" & Key & "'='" &
                                Val.Str_Val & "'");
         when Bool_Prop =>
            Nodes.Print_Line ("Got bool property: '" & Key & "'='" &
                              (if Val.Bool_Val then "True" else "False")
                              & "'");
      end case;
   end Set_Property;

end Nodes.Print_Prop;

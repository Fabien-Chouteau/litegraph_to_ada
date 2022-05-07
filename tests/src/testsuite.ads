with Litegraph_To_Ada;
with Nodes;

package Testsuite is

   type Port_Kind is (Port_A, Port_B, Port_C);
   type Category_Kind is (Cat_A, Cat_B, Cat_C);
   type Prop_Kind is (Prop_Int);

   type Link_Data is record
      Val : Integer;
   end record;

   function Shape_For_Port (Kind : Port_Kind) return String
   is (case Kind is
          when Port_A => "ROUND_SHAPE",
          when Port_B => "BOX_SHAPE",
          when Port_C => "ARROW_SHAPE");

   procedure Print_Custom_LG_Property (Kind    : Prop_Kind;
                                       Label   : String;
                                       Default : Integer)
   is null;

   procedure Port_Value (Str     :     String;
                         K       : out Port_Kind;
                         Success : out Boolean);

   procedure Cat_Value (Str     :     String;
                        K       : out Category_Kind;
                        Success : out Boolean);

   package LG2Ada is new Litegraph_To_Ada
     (Port_Kind                => Port_Kind,
      Category_Kind            => Category_Kind,
      Property_Kind            => Prop_Kind,
      Link_Data                => Link_Data,
      Shape_For_Port           => Shape_For_Port,
      Print_Custom_LG_Property => Print_Custom_LG_Property,
      Port_Value               => Port_Value,
      Cat_Value                => Cat_Value,
      Put_Line                 => Nodes.Print_Line);

end Testsuite;

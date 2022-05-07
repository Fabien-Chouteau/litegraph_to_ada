with Litegraph_To_Ada.Bounded_Manager;

with Nodes.Print_A;
with Nodes.Print_B;
with Nodes.Print_C;

with Nodes.Input_A;
with Nodes.Input_B;
with Nodes.Input_C;

with Nodes.Mixer_A;
with Nodes.Mixer_B;
with Nodes.Mixer_C;

with Nodes.Bridge_A_B;
with Nodes.Bridge_B_C;

with Nodes.Split;
with Nodes.Print_Prop;

generic
   Number_Of_Links : Positive;
   Number_Of_Types : Positive;
   Number_Of_Nodes : Positive;
package Testsuite.Manager is

   package Bounded_Manager is new LG2Ada.Bounded_Manager
     (Number_Of_Links,
      Number_Of_Types,
      Number_Of_Nodes);

   package Register_Print_A_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Print_A.Node);
   package Register_Print_B_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Print_B.Node);
   package Register_Print_C_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Print_C.Node);

   package Register_Input_A_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Input_A.Node);
   package Register_Input_B_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Input_B.Node);
   package Register_Input_C_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Input_C.Node);

   package Register_Mixer_A_Node
   is new Bounded_Manager.Node_Type_Register (Nodes.Mixer_A.Node, 10);
   package Register_Mixer_B_Node
   is new Bounded_Manager.Node_Type_Register (Nodes.Mixer_B.Node, 10);
   package Register_Mixer_C_Node
   is new Bounded_Manager.Node_Type_Register (Nodes.Mixer_C.Node, 10);

   package Register_Bridge_A_B_Node
   is new Bounded_Manager.Node_Type_Register (Nodes.Bridge_A_B.Node, 10);
   package Register_Bridge_B_C_Node
   is new Bounded_Manager.Node_Type_Register (Nodes.Bridge_B_C.Node, 10);

   package Register_Split_Node
   is new Bounded_Manager.Node_Type_Register (Nodes.Split.Node, 10);

   package Register_Print_Prop_Node
   is new Bounded_Manager.Singleton_Node_Register (Nodes.Print_Prop.Node);

end Testsuite.Manager;

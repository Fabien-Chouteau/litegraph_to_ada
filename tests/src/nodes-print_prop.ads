with Testsuite; use Testsuite;

package Nodes.Print_Prop is
   use Testsuite.LG2Ada;

   subtype Parent is LG2Ada.Node;

   type Node
   is new Parent
   with null record;

   overriding
   function Category (This : Node) return Category_Kind
   is (Cat_A);

   overriding
   function Name (This : Node) return String
   is ("print_prop");

   overriding
   function In_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (Invalid_Port);

   overriding
   function Out_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (Invalid_Port);

   overriding
   function Get_Property_Info (This : Node; Prop : Property_Id)
                               return Property_Info
   is (Invalid_Property);

   overriding
   procedure Set_Property (This : in out Node;
                           Key  :        String;
                           Val  :        Property_Value);

   overriding
   procedure Receive (This : in out Node;
                      Port :        Port_Id;
                      Data :        Link_Data)
   is null;

end Nodes.Print_Prop;

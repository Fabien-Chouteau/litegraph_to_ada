with Testsuite; use Testsuite;

generic
   Cat_K  : Testsuite.Category_Kind;
   Port_K : Testsuite.Port_Kind;
   Node_Name : String;
package Nodes.Gen_Input is
   use Testsuite.LG2Ada;

   subtype Parent is LG2Ada.Node;

   type Node
   is new Parent
   with null record;

   procedure Push (This : Node; Data : Link_Data);

   overriding
   function Category (This : Node) return Category_Kind
   is (Cat_K);

   overriding
   function Name (This : Node) return String
   is (Node_Name);

   overriding
   function In_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (Invalid_Port);

   overriding
   function Out_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (case Port is
          when 0 => (3, Port_K, "Out"),
          when others => Invalid_Port);

   overriding
   function Get_Property_Info (This : Node; Prop : Property_Id)
                               return Property_Info
   is (Invalid_Property);

   overriding
   procedure Receive (This : in out Node;
                      Port :        Port_Id;
                      Data :        Link_Data)
   is null;

end Nodes.Gen_Input;

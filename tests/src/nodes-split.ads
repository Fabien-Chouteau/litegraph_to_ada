with Testsuite; use Testsuite;

package Nodes.Split is
   use Testsuite.LG2Ada;

   subtype Parent is LG2Ada.Node;

   type Node
   is new Parent
   with record
      Split_Point : Integer := 0;
   end record;

   overriding
   function Category (This : Node) return Category_Kind
   is (Cat_A);

   overriding
   function Name (This : Node) return String
   is ("split_b_c");

   overriding
   function In_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (case Port is
          when 0 => (2, Port_A, "In"),
          when others => Invalid_Port);

   overriding
   function Out_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (case Port is
          when 0 => (3, Port_B, "Out"),
          when 1 => (3, Port_C, "Out"),
          when others => Invalid_Port);

   overriding
   function Get_Property_Info (This : Node; Prop : Property_Id)
                               return Property_Info
   is (case Prop is
          when 0 => (11, Prop_Int, "split_point", 0),
          when others => Invalid_Property);

   overriding
   procedure Set_Property (This : in out Node; Key : String; Val : Integer);

   overriding
   procedure Receive (This : in out Node;
                      Port :        Port_Id;
                      Data :        Link_Data);

end Nodes.Split;

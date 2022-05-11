with Testsuite; use Testsuite;

package Nodes.Widgets is
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
   is ("widgets");

   overriding
   function In_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (case Port is
          when 0 => (2, Port_A, "In"),
          when others => Invalid_Port);

   overriding
   function Out_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is (case Port is
          when 0 => (3, Port_A, "Out"),
          when others => Invalid_Port);

   overriding
   function Get_Property_Info (This : Node; Prop : Property_Id)
                               return Property_Info
   is (case Prop is
          when 0 => (13, Int_Prop, "number_widget", Number,
                     Int_Min     => -100,
                     Int_Max     => 100,
                     Int_Default => 0),
          when 1 => (13, Int_Prop, "slider_widget", Slider,
                     Int_Min     => -100,
                     Int_Max     => 100,
                     Int_Default => 0),
          when 2 => (12, Int_Prop, "combo_widget", Combo,
                     Int_Min     => -5,
                     Int_Max     => 5,
                     Int_Default => 0),
          when 3 => (11, Str_Prop, "text_widget", Text),
          when 4 => (13, Bool_Prop, "toggle_widget", Toggle, False),
          when others => Invalid_Property);

   overriding
   procedure Set_Property (This : in out Node;
                           Key  :        String;
                           Val  :        Property_Value);

   overriding
   procedure Receive (This : in out Node;
                      Port :        Port_Id;
                      Data :        Link_Data);

end Nodes.Widgets;

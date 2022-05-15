generic
   type Port_Kind is (<>);
   type Category_Kind is (<>);

   type Link_Data (<>) is limited private;

   with function Shape_For_Port (Kind : Port_Kind) return String;

   with procedure Port_Value (Str     :     String;
                              K       : out Port_Kind;
                              Success : out Boolean);

   with procedure Cat_Value (Str     :     String;
                             K       : out Category_Kind;
                             Success : out Boolean);

   with procedure Put_Line (Str : String);

package Litegraph_To_Ada is

   pragma Unreferenced (Port_Value, Cat_Value);

   type Port_Id is new Natural;
   type Port_Info (Label_Len : Natural) is record
      Kind  : Port_Kind;
      Label : String (1 .. Label_Len);
   end record;

   Invalid_Port : constant Port_Info := (0, Port_Kind'First,
                                         (others => ASCII.NUL));

   type Property_Id is new Natural;
   type Property_Kind is (Int_Prop, Str_Prop, Bool_Prop);

   type Int_Widget_Kind is (None, Number, Slider, Combo);
   type Str_Widget_Kind is (None, Text);
   type Bool_Widget_Kind is (None, Toggle);

   type Property_Info (Label_Len : Natural; Kind : Property_Kind) is record
      Label   : String (1 .. Label_Len);
      case Kind is
         when Int_Prop =>
            Int_Widget : Int_Widget_Kind;
            Int_Min, Int_Max : Integer;
            Int_Default : Integer;
         when Str_Prop =>
            Str_Widget : Str_Widget_Kind;
         when Bool_Prop =>
            Bool_Widget : Bool_Widget_Kind;
            Bool_Default : Boolean;
      end case;
   end record;

   Invalid_Property : constant Property_Info := (0, Property_Kind'First,
                                                 others => <>);

   type Node
   is abstract tagged limited
   private;

   type Any_Node_Acc is access all Node'Class;

   function Category (This : Node) return Category_Kind
   is abstract;

   function Name (This : Node) return String
   is abstract;

   function Out_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is abstract;

   function In_Port_Info (This : Node; Port : Port_Id) return Port_Info
   is abstract;

   function Get_Property_Info (This : Node; Prop : Property_Id)
                               return Property_Info
   is abstract;

   type Property_Value (Kind : Property_Kind; Str_Len : Natural) is record
      case Kind is
      when Int_Prop =>
         Int_Val : Integer;
      when Bool_Prop =>
         Bool_Val : Boolean;
      when Str_Prop =>
         Str_Val : String (1 .. Str_Len);
      end case;
   end record;

   procedure Set_Property (This : in out Node;
                           Id   :        Property_Id;
                           Val  :        Property_Value)
   is null;

   procedure On_Start (This : in out Node)
   is null;

   procedure Send (This : Node;
                   Port : Port_Id;
                   Data : Link_Data);

   procedure Receive (This : in out Node;
                      Port :        Port_Id;
                      Data :        Link_Data)
   is abstract;

   procedure Print_LG_Definition (This : Node'Class);
   --  Print JavaScript code that defines a LiteGraph node

private

   type Link;
   type Any_Link_Acc is access all Link;

   type Link is record
      Kind        : Port_Kind;
      Org_Port    : Port_Id;
      Target      : Any_Node_Acc;
      Target_Port : Port_Id;
      Next        : Any_Link_Acc := null;
   end record;

   type Node
   is abstract tagged limited
           record
              Output_Links : Any_Link_Acc := null;
           end record;

   type Connection_Result is (Ok,
                              Wrong_Org_Kind, Wrong_Tar_Kind,
                              Invalid_In_Port, Invalid_Out_Port);

   procedure Connect (This     : in out   Node;
                      L        : not null Any_Link_Acc;
                      Kind     :          Port_Kind;
                      Org_Port :          Port_Id;
                      Target   : not null Any_Node_Acc;
                      Tar_Port :          Port_Id;
                      Result   :    out   Connection_Result);

   procedure Reset_Links (This : in out Node);

end Litegraph_To_Ada;

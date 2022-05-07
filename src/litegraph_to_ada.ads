generic
   type Port_Kind is (<>);
   type Category_Kind is (<>);
   type Property_Kind is (<>);

   type Link_Data (<>) is limited private;

   with function Shape_For_Port (Kind : Port_Kind) return String;

   with procedure Print_Custom_LG_Property (Kind    : Property_Kind;
                                            Label   : String;
                                            Default : Integer);

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
   type Property_Info (Label_Len : Natural) is record
      Kind    : Property_Kind;
      Label   : String (1 .. Label_Len);
      Default : Integer;
   end record;

   Invalid_Property : constant Property_Info := (0, Property_Kind'First,
                                                 (others => ASCII.NUL), 0);

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

   procedure Set_Property (This : in out Node; Key : String; Val : Integer)
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

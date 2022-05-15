generic
   Number_Of_Links  : Positive;
   Number_Of_Nodes  : Positive;
   Node_Memory_Size : Positive;
package Litegraph_To_Ada.Bounded_Manager is

   generic
      type T is new Litegraph_To_Ada.Node with private;
   package Node_Type_Register is
   end Node_Type_Register;

   generic
      type T is new Litegraph_To_Ada.Node with private;
   package Singleton_Node_Register is
      Singleton : aliased T;
   end Singleton_Node_Register;

   type Load_Result is (Ok,
                        Cannot_Modify_Graph_After_Start,
                        Invalid_Category,
                        Invalid_Node_Type,
                        Invalid_Node_Id,
                        Missing_Node_Id,
                        Duplicate_Node_Id,

                        Unknown_Node_Type,

                        Invalid_Link_Type,

                        Invalid_Port_Id,

                        Invalid_Connection_Wrong_Org_Kind,
                        Invalid_Connection_Wrong_Tar_Kind,

                        Invalid_Connection_In_Port,
                        Invalid_Connection_Out_Port,

                        Unknown_Property,
                        Invalid_Property_Format,
                        Invalid_Property_Kind,
                        Invalid_Property_Value,

                        Max_Link_Reached,
                        Max_Node_Reached,
                        Node_Memory_Exhausted
                       );

   procedure Load_Config_Line (Line : String; Result : out Load_Result);
   --  Parse and load the give graph config line

   procedure Start;
   --  Start the graph. This procedure will call On_Start for all nodes, which
   --  may trigger transfers. The graph is not modifiable after this point.

   procedure Reset;
   --  Remove all nodes an links

   procedure Print_LG_Definitions;

   function Result_String (Result : Load_Result) return String
   is (case Result is
          when Ok => "ok",
          when Cannot_Modify_Graph_After_Start =>
             "cannot modify graph after start",
          when Invalid_Category  => "invalid node category",
          when Invalid_Node_Type => "invalid node type",
          when Invalid_Node_Id   => "invalid node id",
          when Missing_Node_Id   => "missing node id",
          when Duplicate_Node_Id => "duplicate node id",
          when Unknown_Node_Type => "unknown node type",
          when Invalid_Link_Type => "invalid link type",
          when Invalid_Port_Id   => "invalid port id",
          when Invalid_Connection_Wrong_Org_Kind =>
             "invalid connection, origin type",
          when Invalid_Connection_Wrong_Tar_Kind =>
             "invalid connection, target type",
          when Invalid_Connection_In_Port =>
             "invalid connection, input port",
          when Invalid_Connection_Out_Port =>
             "invalid connection, output port",

          when Unknown_Property => "unknown property",
          when Invalid_Property_Format => "invalid property format",
          when Invalid_Property_Kind => "invalid property kind",
          when Invalid_Property_Value => "invalid property value",

          when Max_Link_Reached => "max number of links reached",
          when Max_Node_Reached => "max number of nodes reached",
          when Node_Memory_Exhausted => "node memory exhausted");

private

   type Node_Allocator;
   type Acc_Any_Node_Allocator is access all Node_Allocator'Class;
   type Node_Allocator is abstract tagged record
      Next : Acc_Any_Node_Allocator := null;
   end record;

   function Category (This : Node_Allocator) return Category_Kind
   is abstract;
   function Name (This : Node_Allocator) return String
   is abstract;
   procedure Allocate (This : in out Node_Allocator; Acc : out Any_Node_Acc)
   is abstract;
   procedure Reset (This : in out Node_Allocator)
   is abstract;
   procedure Print_LG_Definition (This : Node_Allocator)
   is abstract;

   Node_Allocators : Acc_Any_Node_Allocator := null;

   procedure Register_Node_Allocator (This : not null Acc_Any_Node_Allocator);

   function Find_Allocator (Cat : Category_Kind; Name : String)
                            return Acc_Any_Node_Allocator;
   --  Return null if no allocator can be found

   Links : array (1 .. Number_Of_Links) of aliased Link;
   Next_Free_Link : Positive := Links'First;

   Nodes : array (1 .. Number_Of_Nodes) of Any_Node_Acc :=
     (others => null);

end Litegraph_To_Ada.Bounded_Manager;

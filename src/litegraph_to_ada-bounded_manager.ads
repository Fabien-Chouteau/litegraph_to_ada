generic
   Number_Of_Links : Positive;
   Number_Of_Types : Positive;
   Number_Of_Nodes : Positive;
package Litegraph_To_Ada.Bounded_Manager is

   generic
      type T is new Litegraph_To_Ada.Node with private;
      Number_Of_Nodes : Positive;
   package Node_Type_Register is
   end Node_Type_Register;

   generic
      type T is new Litegraph_To_Ada.Node with private;
   package Singleton_Node_Register is
      Singleton : aliased T;
   end Singleton_Node_Register;

   procedure Load_Config_Line (Line : String);
   --  Parse and load the give graph config line

   procedure Reset;
   --  Remove all nodes an links

   procedure Print_LG_Definitions;

private

   type Node_Allocator is abstract tagged null record;
   type Acc_Any_Node_Allocator is access all Node_Allocator'Class;

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

   Node_Allocators : array (1 .. Number_Of_Types) of Acc_Any_Node_Allocator :=
     (others => null);
   Next_Free_Allocator : Positive := Node_Allocators'First;

   procedure Register_Node_Allocator (This : not null Acc_Any_Node_Allocator);

   function Find_Allocator (Cat : Category_Kind; Name : String)
                            return Acc_Any_Node_Allocator;
   --  Return null if no allocator can be found

   Links : array (1 .. Number_Of_Links) of aliased Link;
   Next_Free_Link : Positive := Links'First;

   Nodes : array (1 .. Number_Of_Nodes) of Any_Node_Acc :=
     (others => null);

end Litegraph_To_Ada.Bounded_Manager;

package body Litegraph_To_Ada.Bounded_Manager is

   ------------------------
   -- Node_Type_Register --
   ------------------------

   package body Node_Type_Register is

      Nodes : array (1 .. Number_Of_Nodes) of aliased T;
      Next_Free_Node : Positive := Nodes'First;

      type T_Allocator is new Node_Allocator with null record;

      overriding
      procedure Reset (This : in out T_Allocator);
      overriding
      procedure Print_LG_Definition (This : T_Allocator);

      --------------
      -- Category --
      --------------

      overriding
      function Category (This : T_Allocator) return Category_Kind
      is (Nodes (Nodes'First).Category);

      ----------
      -- Name --
      ----------

      overriding
      function Name (This : T_Allocator) return String
      is (Nodes (Nodes'First).Name);

      --------------
      -- Allocate --
      --------------

      overriding
      procedure Allocate (This : in out T_Allocator; Acc : out Any_Node_Acc) is
      begin
         if Next_Free_Node in Nodes'Range then
            Acc := Nodes (Next_Free_Node)'Access;
            Next_Free_Node := Next_Free_Node + 1;
         else
            Acc := null;
         end if;
      end Allocate;

      -----------
      -- Reset --
      -----------

      overriding
      procedure Reset (This : in out T_Allocator) is
      begin
         for N of Nodes (Nodes'First .. Next_Free_Node - 1) loop
            Litegraph_To_Ada.Node (N).Reset_Links;
         end loop;

         Next_Free_Node := Nodes'First;
      end Reset;

      -------------------------
      -- Print_LG_Definition --
      -------------------------

      overriding
      procedure Print_LG_Definition (This : T_Allocator) is
      begin
         Litegraph_To_Ada.Print_LG_Definition (Nodes (Nodes'First));
      end Print_LG_Definition;

      Singleton : aliased T_Allocator;
   begin
      Register_Node_Allocator (Singleton'Access);
   end Node_Type_Register;

   -----------------------------
   -- Singleton_Node_Register --
   -----------------------------

   package body Singleton_Node_Register is

      type T_Allocator is new Node_Allocator with null record;

      overriding
      procedure Reset (This : in out T_Allocator);

      overriding
      procedure Print_LG_Definition (This : T_Allocator);

      --------------
      -- Category --
      --------------

      overriding
      function Category (This : T_Allocator) return Category_Kind
      is (Singleton.Category);

      ----------
      -- Name --
      ----------

      overriding
      function Name (This : T_Allocator) return String
      is (Singleton.Name);

      --------------
      -- Allocate --
      --------------

      overriding
      procedure Allocate (This : in out T_Allocator; Acc : out Any_Node_Acc) is
      begin
         Acc := Singleton'Access;
      end Allocate;

      -----------
      -- Reset --
      -----------

      overriding
      procedure Reset (This : in out T_Allocator) is
      begin
         Litegraph_To_Ada.Node (Singleton).Reset_Links;
      end Reset;


      -------------------------
      -- Print_LG_Definition --
      -------------------------

      overriding
      procedure Print_LG_Definition (This : T_Allocator) is
      begin
         Litegraph_To_Ada.Print_LG_Definition (Singleton);
      end Print_LG_Definition;

      Singleton_Allocator : aliased T_Allocator;
   begin
      Register_Node_Allocator (Singleton_Allocator'Access);
   end Singleton_Node_Register;

   -----------------------------
   -- Register_Node_Allocator --
   -----------------------------

   procedure Register_Node_Allocator (This : not null Acc_Any_Node_Allocator)
   is
   begin
      if Next_Free_Allocator in Node_Allocators'Range then
         Node_Allocators (Next_Free_Allocator) := This;
         Next_Free_Allocator := Next_Free_Allocator + 1;
      else
         raise Program_Error;
      end if;
   end Register_Node_Allocator;

   --------------------
   -- Find_Allocator --
   --------------------

   function Find_Allocator (Cat : Category_Kind; Name : String)
                            return Acc_Any_Node_Allocator
   is
   begin
      for Alloc
        of Node_Allocators (Node_Allocators'First .. Next_Free_Allocator - 1)
      loop
         if Alloc.Category = Cat and then Alloc.Name = Name then
            return Alloc;
         end if;
      end loop;

      return null;
   end Find_Allocator;

   ----------------------
   -- Load_Config_Line --
   ----------------------

   procedure Load_Config_Line (Line : String) is

      type String_Slice is record
         F : Integer; --  First index of the slice
         L : Integer; --  Last index of the slice
      end record;

      function Valid (S : String_Slice) return Boolean
      is (S.F <= S.L and then S.F in Line'Range and then S.L in Line'Range);

      function To_Str (S : String_Slice) return String
      is (if not Valid (S) then "" else Line (S.F .. S.L));

      Prop_Slice : String_Slice := (Line'First, Line'First - 2);
      --  Property definition

      Key_Slice : String_Slice := (1, 0);
      --  Key of the property

      Val_Slice : String_Slice := (1, 0);
      --  Value of the property

      -----------------
      -- Parse_Slice --
      -----------------

      procedure Parse_Slice (From : Natural;
                             Delim : Character;
                             Slice : out String_Slice)
      is
      begin
         Slice.F := From;
         Slice.L := From;

         while Slice.L in Line'Range and then Line (Slice.L) /= Delim loop
            Slice.L := Slice.L + 1;
         end loop;

         if Slice.L not in Line'Range then
            Slice := (1, 0);
         else
            Slice.L := Slice.L - 1;
         end if;
      end Parse_Slice;

      -------------------------
      -- Parse_Next_Property --
      -------------------------

      procedure Parse_Next_Property is
      begin
         Key_Slice := (1, 0);
         Val_Slice := (1, 0);

         Parse_Slice (Prop_Slice.L + 2, ':', Prop_Slice);

         if not Valid (Prop_Slice) then
            return;
         end if;

         Parse_Slice (Prop_Slice.F, '=', Key_Slice);

         if not Valid (Key_Slice) then
            return;
         else
            Val_Slice.F := Key_Slice.L + 2;
            Val_Slice.L := Prop_Slice.L;
         end if;

         --  Put_Line ("Prop: " & To_Str (Prop_Slice));
         --  Put_Line ("Key: " & To_Str (Key_Slice));
         --  Put_Line ("Value: " & To_Str (Val_Slice));
      end Parse_Next_Property;

      ----------------------
      -- Load_Node_Config --
      ----------------------

      procedure Load_Node_Config is
         Cat_Slice : String_Slice;
         Name_Slice : String_Slice;

         Cat : Category_Kind;
         Alloc : Acc_Any_Node_Allocator;
         N : Any_Node_Acc := null;
         Id : Natural := Nodes'First - 1;

         Success : Boolean;
      begin

         Parse_Slice (Val_Slice.F, '/', Cat_Slice);

         if not Valid (Cat_Slice) then
            raise Program_Error;
         else
            Name_Slice.F := Cat_Slice.L + 2;
            Name_Slice.L := Val_Slice.L;
         end if;

         Cat_Value (To_Str (Cat_Slice), Cat, Success);
         if not Success then
            raise Program_Error with "Invalid category";
         end if;

         Alloc := Find_Allocator (Cat, To_Str (Name_Slice));

         if Alloc = null then
            raise Program_Error;
         end if;

         Alloc.Allocate (N);

         loop
            Parse_Next_Property;

            exit when not Valid (Key_Slice);

            declare
               Key : constant String := To_Str (Key_Slice);
               Val : constant String := To_Str (Val_Slice);
            begin
               if Key = "id" then
                  Id := Natural'Value (Val);

                  if Id not in Nodes'Range then
                     raise Program_Error with "Invalid node id";
                  end if;

                  if Nodes (Id) /= null then
                     raise Program_Error with "Duplicate node id";
                  end if;

                  Nodes (Id) := N;
               elsif Key = "pox_x" or else Key = "pos_y" then
                  null; -- Ignore position
               else
                  N.Set_Property (Key, Integer'Value (Val));
               end if;
            end;

            if Id not in Nodes'Range then
               raise Program_Error with "Missing Id";
            end if;
         end loop;
      end Load_Node_Config;

      --------------------
      -- Parse_Port_Def --
      --------------------

      procedure Parse_Port_Def (N : out Any_Node_Acc; P : out Port_Id) is
      begin
         Parse_Next_Property;

         if not Valid (Key_Slice) then
            raise Program_Error;
         end if;

         if not Valid (Val_Slice) then
            raise Program_Error;
         end if;

         declare
            Node_Id : constant Integer := Integer'Value (To_Str (Key_Slice));
            P_Id : constant Integer := Integer'Value (To_Str (Val_Slice));
         begin
            if Node_Id not in Nodes'Range then
               raise Program_Error with "Invalid node Id";
            else
               N := Nodes (Node_Id);
            end if;

            if N = null then
               raise Program_Error with "Unknown node Id";
            end if;

            if P_Id not in Integer (Port_Id'First) .. Integer (Port_Id'Last)
            then
               raise Program_Error;
            else
               P := Port_Id (P_Id);
            end if;
         end;
      end Parse_Port_Def;

      ----------------------
      -- Load_Link_Config --
      ----------------------

      procedure Load_Link_Config is
         Kind_Str : constant String := To_Str (Val_Slice);
         Kind : Port_Kind;

         Org_Node : Any_Node_Acc := null;
         Org_Port : Port_Id;

         Tar_Node : Any_Node_Acc := null;
         Tar_Port : Port_Id;

         Link_Id  : constant Natural := Next_Free_Link;

         Result : Connection_Result;
         Success : Boolean;
      begin

         if Link_Id not in Links'Range then
            raise Program_Error;
         else
            Next_Free_Link := Next_Free_Link + 1;
         end if;

         Port_Value (Kind_Str, Kind, Success);
         if not Success then
            raise Program_Error with "Invalid link type";
         end if;

         Parse_Port_Def (Org_Node, Org_Port);
         Parse_Port_Def (Tar_Node, Tar_Port);

         Org_Node.Connect (L        => Links (Link_Id)'Access,
                           Kind     => Kind,
                           Org_Port => Org_Port,
                           Target   => Tar_Node,
                           Tar_Port => Tar_Port,
                           Result   => Result);

         if Result /= Ok then
            raise Program_Error with "Invalid connection: '" & Line & "' : " &
              Result'Img;
         end if;
      end Load_Link_Config;

   begin
      Parse_Next_Property;

      if To_Str (Key_Slice) = "node" then
         Load_Node_Config;
      elsif To_Str (Key_Slice) = "link" then
         Load_Link_Config;
      else
         raise Program_Error;
      end if;
   end Load_Config_Line;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin

      for Alloc
        of Node_Allocators (Node_Allocators'First .. Next_Free_Allocator - 1)
      loop
         Alloc.Reset;
      end loop;

      Next_Free_Link := Links'First;
   end Reset;

   --------------------------
   -- Print_LG_Definitions --
   --------------------------

   procedure Print_LG_Definitions is
   begin
      for Alloc of Node_Allocators (1 .. Next_Free_Allocator - 1) loop
         Alloc.Print_LG_Definition;
      end loop;
   end Print_LG_Definitions;

end Litegraph_To_Ada.Bounded_Manager;

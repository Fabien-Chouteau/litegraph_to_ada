package body Litegraph_To_Ada.Bounded_Manager is

   Started : Boolean := False;

   ----------------
   -- Str_To_Int --
   ----------------

   procedure Str_To_Int (Str     :     String;
                         Val     : out Integer;
                         Success : out Boolean)
   is

      Mult : Integer := 1;
   begin
      Val := 0;

      if Str'Length = 0 then
         Success := False;
         return;
      end if;

      if Str (Str'First) = '-' then
         Str_To_Int (Str (Str'First + 1 .. Str'Last), Val, Success);
         Val := -Val;
         return;
      end if;

      for C of reverse Str loop
         if C not in '0' .. '9' then
            Success := False;
            return;
         end if;

         Val := Val + Mult * Integer (Character'Pos (C) - Character'Pos ('0'));
         Mult := Mult * 10;
      end loop;

      Success := True;
   end Str_To_Int;

   ---------------------
   -- Str_To_Prop_Val --
   ---------------------

   function Str_To_Prop_Val (Str     :     String;
                             Success : out Boolean)
                             return Property_Value
   is
   begin
      Success := True;

      if Str'Length = 0 then
         Success := False;
         return (Str_Prop, 0, "");
      end if;

      case Str (Str'First) is
         when '0' .. '9' | '-' =>
            declare
               Int : Integer;
            begin
               Str_To_Int (Str, Int, Success);
               return (Int_Prop, 0, Int);
            end;

         when 't' | 'f' =>
            if Str = "true" then
               return (Bool_Prop, 0, True);
            elsif Str = "false" then
               return (Bool_Prop, 0, False);
            else
               Success := False;
               return (Bool_Prop, 0, False);
            end if;

         when '"' =>
            if Str'Length < 2 or else Str (Str'Last) /= '"' then
               Success := False;
               return (Str_Prop, 0, "");
            else

               declare
                  Result : String := Str (Str'First + 1 .. Str'Last - 1);
                  Src : Natural := Result'First;
                  Dst : Natural := Result'First;
               begin
                  while Src in Result'Range loop

                     if Result (Src) = '\' then
                        if Src < Result'Last
                          and then
                            Str (Src + 1) in '"' | '\'
                        then
                           Src := Src + 1;
                        else
                           Success := False;
                           return (Str_Prop, 0, "");
                        end if;
                     end if;

                     Result (Dst) := Result (Src);
                     Dst := Dst + 1;
                     Src := Src + 1;
                  end loop;

                  return (Str_Prop, Dst - Result'First,
                          Result (Result'First .. Dst - 1));
               end;
            end if;

         when others =>
            Success := False;
            return (Str_Prop, 0, "");
      end case;
   end Str_To_Prop_Val;

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

   procedure Load_Config_Line (Line : String; Result : out Load_Result) is

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
         Id : Natural := Nodes'Last + 1;

         Success : Boolean;
      begin

         Parse_Slice (Val_Slice.F, '/', Cat_Slice);

         if not Valid (Cat_Slice) then
            Result := Invalid_Category;
            return;
         else
            Name_Slice.F := Cat_Slice.L + 2;
            Name_Slice.L := Val_Slice.L;
         end if;

         Cat_Value (To_Str (Cat_Slice), Cat, Success);
         if not Success then
            Result := Invalid_Category;
            return;
         end if;

         Alloc := Find_Allocator (Cat, To_Str (Name_Slice));

         if Alloc = null then
            Result := Unknown_Node_Type;
            return;
         end if;

         Alloc.Allocate (N);

         if N = null then
            Result := Max_Node_For_Type_Reached;
            return;
         end if;

         loop
            Parse_Next_Property;

            exit when not Valid (Key_Slice);

            declare
               Key : constant String := To_Str (Key_Slice);
               Val : constant String := To_Str (Val_Slice);
               Success : Boolean;
            begin
               if Key = "id" then

                  Str_To_Int (Val, Id, Success);
                  if not Success then
                     Result := Invalid_Node_Id;
                     return;
                  end if;

                  if Id not in Nodes'Range then
                     Result := Max_Node_Reached;
                     return;
                  end if;

                  if Nodes (Id) /= null then
                     Result := Duplicate_Node_Id;
                     return;
                  end if;

                  Nodes (Id) := N;

               elsif Key = "pox_x" or else Key = "pos_y" then
                  null; -- Ignore position

               else
                  declare
                     Prop_Val : constant Property_Value
                       :=  Str_To_Prop_Val (Val, Success);
                  begin
                     if not Success then
                        Result := Invalid_Property_Format;
                        return;
                     end if;
                     N.Set_Property (Key, Prop_Val);
                  end;
               end if;
            end;
         end loop;

         if Id not in Nodes'Range then
            Result := Missing_Node_Id;
         end if;
      end Load_Node_Config;

      --------------------
      -- Parse_Port_Def --
      --------------------

      procedure Parse_Port_Def (N : out Any_Node_Acc; P : out Port_Id) is
      begin
         Parse_Next_Property;

         if not Valid (Key_Slice) then
            Result := Invalid_Property_Format;
            P := 0;
            return;
         end if;

         if not Valid (Val_Slice) then
            Result := Invalid_Property_Format;
            return;
         end if;

         declare
            Node_Id : Integer;
            P_Id    : Integer;
            Success : Boolean;
         begin
            Str_To_Int (To_Str (Key_Slice), Node_Id, Success);
            if not Success or else Node_Id not in Nodes'Range then
               Result := Invalid_Node_Id;
               return;
            else
               N := Nodes (Node_Id);
            end if;

            if N = null then
               Result := Invalid_Node_Id;
               return;
            end if;

            Str_To_Int (To_Str (Val_Slice), P_Id, Success);
            if not Success
              or else
                P_Id not in Integer (Port_Id'First) .. Integer (Port_Id'Last)
            then
               Result := Invalid_Port_Id;
               return;
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

         C_Result : Connection_Result;
         Success : Boolean;
      begin

         if Link_Id not in Links'Range then
            Result := Max_Link_Reached;
            return;
         else
            Next_Free_Link := Next_Free_Link + 1;
         end if;

         Port_Value (Kind_Str, Kind, Success);
         if not Success then
            Result := Invalid_Link_Type;
            return;
         end if;

         Parse_Port_Def (Org_Node, Org_Port);
         if Result /= Ok then
            return;
         end if;

         Parse_Port_Def (Tar_Node, Tar_Port);
         if Result /= Ok then
            return;
         end if;

         Org_Node.Connect (L        => Links (Link_Id)'Access,
                           Kind     => Kind,
                           Org_Port => Org_Port,
                           Target   => Tar_Node,
                           Tar_Port => Tar_Port,
                           Result   => C_Result);

         case C_Result is
            when Ok => null;
            when Wrong_Org_Kind => Result := Invalid_Connection_Wrong_Org_Kind;
            when Wrong_Tar_Kind => Result := Invalid_Connection_Wrong_Tar_Kind;
            when Invalid_In_Port => Result := Invalid_Connection_In_Port;
            when Invalid_Out_Port => Result := Invalid_Connection_Out_Port;
         end case;
      end Load_Link_Config;

   begin

      if Started then
         Result := Cannot_Modify_Graph_After_Start;
         return;
      end if;

      Result := Ok;

      Parse_Next_Property;
      if Result /= Ok then
         return;
      end if;

      if To_Str (Key_Slice) = "node" then
         Load_Node_Config;
      elsif To_Str (Key_Slice) = "link" then
         Load_Link_Config;
      else
         raise Program_Error;
      end if;
   end Load_Config_Line;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      for N of Nodes loop
         if N /= null then
            N.On_Start;
         end if;
      end loop;
      Started := True;
   end Start;

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

      for N of Nodes loop
         N := null;
      end loop;

      Next_Free_Link := Links'First;

      Started := False;
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

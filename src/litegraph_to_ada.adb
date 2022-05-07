package body Litegraph_To_Ada is

   subtype Dispatch is Node'Class;

   -------------
   -- Connect --
   -------------

   procedure Connect (This     : in out   Node;
                      L        : not null Any_Link_Acc;
                      Kind     :          Port_Kind;
                      Org_Port :          Port_Id;
                      Target   : not null Any_Node_Acc;
                      Tar_Port :          Port_Id;
                      Result   :    out   Connection_Result)
   is
      Org_Info : constant Port_Info :=
        Dispatch (This).Out_Port_Info (Org_Port);
      Tar_Info : constant Port_Info := Target.In_Port_Info (Tar_Port);
   begin
      if Org_Info = Invalid_Port then
         Result := Invalid_Out_Port;
         return;
      end if;

      if Tar_Info = Invalid_Port then
         Result := Invalid_In_Port;
         return;
      end if;

      if Org_Info.Kind /= Kind then
         Result := Wrong_Org_Kind;
         return;
      end if;

      if Tar_Info.Kind /= Kind then
         Result := Wrong_Tar_Kind;
         return;
      end if;

      L.Kind := Kind;
      L.Org_Port := Org_Port;
      L.Target := Target;
      L.Target_Port := Tar_Port;
      L.Next := This.Output_Links;
      This.Output_Links := L;
      Result := Ok;
   end Connect;

   -----------------
   -- Reset_Links --
   -----------------

   procedure Reset_Links (This : in out Node) is
   begin
      This.Output_Links := null;
   end Reset_Links;

   ----------
   -- Send --
   ----------

   procedure Send (This : Node; Port : Port_Id; Data : Link_Data) is
      L : Any_Link_Acc := This.Output_Links;
   begin
      while L /= null loop
         if L.Org_Port = Port then
            L.Target.Receive (L.Target_Port, Data);
         end if;
         L := L.Next;
      end loop;
   end Send;

   --------------------
   -- Print_Port_Def --
   --------------------

   procedure Print_Port_Def (This     : Node'Class;
                             Info     : Port_Info;
                             Is_Input : Boolean)
   is
      pragma Unreferenced (This);

      Type_Str : constant String := Info.Kind'Img;
      Shape    : constant String := Shape_For_Port (Info.Kind);

      Func : constant String := (if Is_Input then "addInput" else "addOutput");
   begin

      if Info /= Invalid_Port then

         Put_Line ("  this." & Func & "(""" &
                     Info.Label
                   & """, """ &
                     Type_Str
                   & """, {shape: LiteGraph." & Shape & "});");
      end if;
   end Print_Port_Def;

   --------------------
   -- Print_Property --
   --------------------

   procedure Print_Property (This : Node'Class; Info : Property_Info) is
      pragma Unreferenced (This);
   begin
      Put_Line ("  this.properties[""" & Info.Label & """] = " &
                  Info.Default'Img & ";");

      Print_Custom_LG_Property (Info.Kind, Info.Label, Info.Default);
   end Print_Property;

   -------------------------
   -- Print_LG_Definition --
   -------------------------

   procedure Print_LG_Definition (This : Node'Class) is

      Name : constant String := This.Name;
      Cat  : constant String := This.Category'Img;
      LG_Node_Name : constant String := "Node_" & Cat & "_" & Name;

   begin
      Put_Line ("function " & LG_Node_Name & "()");
      Put_Line ("{");
      Put_Line ("  that = this;");

      for X in Port_Id'Range loop
         declare
            Info : constant Port_Info := This.In_Port_Info (X);
         begin

            exit when Info = Invalid_Port;

            Print_Port_Def (This, Info, Is_Input => True);
         end;
      end loop;

      for X in Port_Id'Range loop
         declare
            Info : constant Port_Info := This.Out_Port_Info (X);
         begin

            exit when Info = Invalid_Port;

            Print_Port_Def (This, Info, Is_Input => False);
         end;
      end loop;

      Put_Line ("  this.properties = {};");
      for X in Property_Id'Range loop
         declare
            Info : constant Property_Info := This.Get_Property_Info (X);
         begin
            exit when Info = Invalid_Property;
            Print_Property (This, Info);
         end;
      end loop;

      Put_Line ("}");
      Put_Line (LG_Node_Name & ".title = """ & Name & """;");

      Put_Line ("LiteGraph.registerNodeType(""" & Cat & "/" & Name & """, " &
                  LG_Node_Name & ");");
   end Print_LG_Definition;

end Litegraph_To_Ada;

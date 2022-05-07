pragma Ada_2012;
package body Testsuite is

   ----------------
   -- Port_Value --
   ----------------

   procedure Port_Value (Str     :     String;
                         K       : out Port_Kind;
                         Success : out Boolean)
   is
   begin
      K := Port_Kind'Value (Str);
      Success := True;
   exception
      when others =>
         Success := False;
   end Port_Value;

   ---------------
   -- Cat_Value --
   ---------------

   procedure Cat_Value (Str     :     String;
                        K       : out Category_Kind;
                        Success : out Boolean)
   is
   begin
      K := Category_Kind'Value (Str);
      Success := True;
   exception
      when others =>
         Success := False;
   end Cat_Value;

end Testsuite;

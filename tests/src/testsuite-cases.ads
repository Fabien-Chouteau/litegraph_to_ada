with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Testsuite.Cases is

   package Test_Cases_List
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String);

   function Find_Cases return Test_Cases_List.List;

   procedure Run_Case (Dirname : String);

   procedure Run_Cases (List : Test_Cases_List.List);

end Testsuite.Cases;

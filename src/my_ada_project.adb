with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BSON;                 use BSON;

procedure My_Ada_Project is
   Doc     : BSON_Document_Type;
   Sub_Doc : BSON_Document_Type;
begin
   --  Initialize documents
   Init_Document (Doc);
   Init_Document (Sub_Doc);

   --  Document principal
   Add_String (Doc, "name", "John Doe");
   Add_Integer (Doc, "age", 30);

   --  Sous-document
   Add_String (Sub_Doc, "city", "New York");
   Add_String (Sub_Doc, "country", "USA");
   Add_Document (Doc, "address", Sub_Doc);

   --  Tableau
   Add_Array (Doc, "hobbies");
   Array_Add_Value (Doc, "hobbies",
      (Kind => BSON_String,
       String_Value => To_Unbounded_String ("reading")));
   Array_Add_Value (Doc, "hobbies",
      (Kind => BSON_String,
       String_Value => To_Unbounded_String ("gaming")));

   --  Validation
   if Is_Valid (Doc) then
      Put_Line (To_JSON (Doc));
   end if;

exception
   when Invalid_BSON_Format =>
      Put_Line ("Document validation failed");
   when others =>
      Put_Line ("An error occurred");
end My_Ada_Project;

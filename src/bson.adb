-- src/bson.adb
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Streams;

package body BSON is
   use Ada.Streams;

   procedure Init_Document (Doc : in out BSON_Document_Type) is
   begin
      Doc.Elements := Value_Maps.Empty_Map;
      Doc.Is_Array := False;
   end Init_Document;

   procedure Add_String
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind         => BSON_String,
           String_Value => To_Unbounded_String (Value));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_String;

   procedure Add_Integer
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : Integer) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind        => BSON_Int32,
           Int32_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Integer;

   procedure Add_Double
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : Long_Float) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind         => BSON_Double,
           Double_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Double;

   procedure Add_Boolean
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : Boolean) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind          => BSON_Boolean,
           Boolean_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Boolean;

   procedure Add_Document
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : BSON_Document_Type) is
      New_Doc   : constant BSON_Document_Access :=
        new BSON_Document_Type'(Value);
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind           => BSON_Document,
           Document_Value => New_Doc);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Document;

   procedure Add_Array
     (Doc : in out BSON_Document_Type;
      Key : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind        => BSON_Array,
           Array_Value => BSON_Arrays.Empty_Vector);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Array;

   procedure Array_Add_Value
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : BSON_Value_Type) is
      Cursor       : Value_Maps.Cursor;
      Array_Access : BSON_Value_Access;
   begin
      Cursor := Doc.Elements.Find (To_Unbounded_String (Key));
      if not Value_Maps.Has_Element (Cursor) then
         raise Invalid_BSON_Type with "Key not found or not an array";
      end if;

      Array_Access := Value_Maps.Element (Cursor);
      if Array_Access.Kind /= BSON_Array then
         raise Invalid_BSON_Type with "Element is not an array";
      end if;

      Array_Access.Array_Value.Append
        (new BSON_Value_Type'(Value));
   end Array_Add_Value;

   function To_JSON (Doc : BSON_Document_Type) return String is
      Result : Unbounded_String := To_Unbounded_String ("{");
      First  : Boolean := True;

      function Format_Value (Value : BSON_Value_Access) return String is
         use Ada.Strings.Fixed;
      begin
         case Value.Kind is
            when BSON_String =>
               return """" & To_String (Value.String_Value) & """";
            when BSON_Int32 =>
               return Trim (Value.Int32_Value'Image, Ada.Strings.Left);
            when BSON_Double =>
               return Trim (Value.Double_Value'Image, Ada.Strings.Left);
            when BSON_Boolean =>
               return (if Value.Boolean_Value then "true" else "false");
            when others =>
               return "null";
         end case;
      end Format_Value;
   begin
      for C in Doc.Elements.Iterate loop
         if not First then
            Append (Result, ", ");
         end if;
         First := False;

         Append (Result, """" & To_String (Value_Maps.Key (C)) & """: ");
         Append (Result, Format_Value (Value_Maps.Element (C)));
      end loop;

      Append (Result, "}");
      return To_String (Result);
   end To_JSON;

   procedure To_Binary
     (Doc    : in out BSON_Document_Type;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      raise Program_Error with "Not implemented";
   end To_Binary;

   function From_Binary (Buffer : Stream_Element_Array)
     return BSON_Document_Type is
      Result : BSON_Document_Type;
   begin
      Init_Document (Result);
      return Result;
   end From_Binary;

   function Is_Valid (Doc : BSON_Document_Type) return Boolean is
   begin
      for C in Doc.Elements.Iterate loop
         declare
            Value : constant BSON_Value_Access := Value_Maps.Element (C);
         begin
            case Value.Kind is
               when BSON_Document =>
                  if not Is_Valid (Value.Document_Value.all) then
                     return False;
                  end if;
               when BSON_Array =>
                  for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                     declare
                        Element : constant BSON_Value_Access := Value.Array_Value (I);
                     begin
                        if Element = null then
                           return False;
                        end if;
                     end;
                  end loop;
               when others =>
                  null;
            end case;
         end;
      end loop;
      return True;
   end Is_Valid;

   procedure Validate (Doc : BSON_Document_Type) is
   begin
      if not Is_Valid (Doc) then
         raise BSON_Validation_Error with "Document validation failed";
      end if;
   end Validate;

   procedure Free_Document (Doc : in out BSON_Document_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (BSON_Value_Type, BSON_Value_Access);
      Temp : BSON_Value_Access;
   begin
      for C in Doc.Elements.Iterate loop
         Temp := Value_Maps.Element (C);
         Free_Value (Temp);
         Free (Temp);
      end loop;
      Doc.Elements.Clear;
   end Free_Document;

   procedure Free_Value (Value : in out BSON_Value_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (BSON_Value_Type, BSON_Value_Access);
      procedure Free_Doc is new Ada.Unchecked_Deallocation
        (BSON_Document_Type, BSON_Document_Access);
   begin
      if Value /= null then
         case Value.Kind is
            when BSON_Document =>
               Free_Document (Value.Document_Value.all);
               Free_Doc (Value.Document_Value);
            when BSON_Array =>
               for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                  declare
                     Elem : BSON_Value_Access := Value.Array_Value (I);
                  begin
                     Free_Value (Elem);
                  end;
               end loop;
               Value.Array_Value.Clear;
            when others =>
               null;
         end case;
         Free (Value);
      end if;
   end Free_Value;

end BSON;
-- src/bson.adb
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Streams;
with Ada.Calendar.Formatting;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Interfaces;

package body BSON with SPARK_Mode => Off is
   use Ada.Streams;
   use Interfaces;

   -- Constantes pour les types BSON
   BSON_TYPE_DOUBLE      : constant := 16#01#;
   BSON_TYPE_STRING      : constant := 16#02#;
   BSON_TYPE_DOCUMENT    : constant := 16#03#;
   BSON_TYPE_ARRAY       : constant := 16#04#;
   BSON_TYPE_BINARY      : constant := 16#05#;
   BSON_TYPE_UNDEFINED   : constant := 16#06#; -- Déprécié
   BSON_TYPE_OBJECTID    : constant := 16#07#;
   BSON_TYPE_BOOLEAN     : constant := 16#08#;
   BSON_TYPE_DATE        : constant := 16#09#;
   BSON_TYPE_NULL        : constant := 16#0A#;
   BSON_TYPE_REGEX       : constant := 16#0B#;
   BSON_TYPE_DBPOINTER   : constant := 16#0C#; -- Déprécié
   BSON_TYPE_JAVASCRIPT  : constant := 16#0D#;
   BSON_TYPE_SYMBOL      : constant := 16#0E#; -- Déprécié
   BSON_TYPE_JAVASCRIPT_W_SCOPE : constant := 16#0F#;
   BSON_TYPE_INT32       : constant := 16#10#;
   BSON_TYPE_TIMESTAMP   : constant := 16#11#;
   BSON_TYPE_INT64       : constant := 16#12#;
   BSON_TYPE_DECIMAL128  : constant := 16#13#;
   BSON_TYPE_MINKEY      : constant := 16#FF#;
   BSON_TYPE_MAXKEY      : constant := 16#7F#;

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

   procedure Add_Long_Integer
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : Long_Long_Integer) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind        => BSON_Int64,
           Int64_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Long_Integer;

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

   procedure Add_Binary
     (Doc : in out BSON_Document_Type;
      Key : String;
      Subtype_Value : BSON_Binary_Subtype;
      Data : Stream_Element_Array) is
      New_Vector : Binary_Data_Vectors.Vector := Binary_Data_Vectors.Empty_Vector;
      New_Value  : BSON_Value_Access;
   begin
      -- Copie des données binaires dans le vecteur
      for I in Data'Range loop
         New_Vector.Append (Data (I));
      end loop;

      New_Value := new BSON_Value_Type'
        (Kind => BSON_Binary,
         Binary_Subtype => Subtype_Value,
         Binary_Data => New_Vector);

      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Binary;

   procedure Add_ObjectId
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : ObjectId_Type) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind => BSON_ObjectId,
           ObjectId_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_ObjectId;

   procedure Add_Date
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : Long_Long_Integer) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind => BSON_Date,
           Date_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Date;

   procedure Array_Add_Value
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : BSON_Value_Type) is
      Cursor       : Value_Maps.Cursor;
      Array_Access : BSON_Value_Access;
      New_Value    : BSON_Value_Access;
   begin
      Cursor := Doc.Elements.Find (To_Unbounded_String (Key));
      if not Value_Maps.Has_Element (Cursor) then
         raise Invalid_BSON_Type with "Key not found or not an array";
      end if;

      Array_Access := Value_Maps.Element (Cursor);
      if Array_Access.Kind /= BSON_Array then
         raise Invalid_BSON_Type with "Element is not an array";
      end if;

      -- Création d'une copie profonde de la valeur
      New_Value := new BSON_Value_Type'(Value);

      Array_Access.Array_Value.Append (New_Value);
   end Array_Add_Value;

   function Hex_Digit_To_Value (Hex : Character) return Natural is
      Hex_Upper : constant Character := Ada.Characters.Handling.To_Upper (Hex);
   begin
      if Hex_Upper in '0' .. '9' then
         return Character'Pos (Hex_Upper) - Character'Pos ('0');
      elsif Hex_Upper in 'A' .. 'F' then
         return Character'Pos (Hex_Upper) - Character'Pos ('A') + 10;
      else
         raise Invalid_BSON_Format with "Invalid hexadecimal character";
      end if;
   end Hex_Digit_To_Value;

   function Hex_To_Stream_Element (Hex : String) return Stream_Element is
      High_Nibble : constant Natural := Hex_Digit_To_Value (Hex (Hex'First));
      Low_Nibble  : constant Natural := Hex_Digit_To_Value (Hex (Hex'First + 1));
   begin
      return Stream_Element (High_Nibble * 16 + Low_Nibble);
   end Hex_To_Stream_Element;

   function To_JSON (Doc : BSON_Document_Type) return String is
      Result : Unbounded_String;
      First  : Boolean := True;

      function Format_Value (Value : BSON_Value_Access) return String is
         use Ada.Strings.Fixed;
      begin
         case Value.Kind is
            when BSON_String =>
               return """" & To_String (Value.String_Value) & """";
            when BSON_Int32 =>
               return Trim (Value.Int32_Value'Image, Ada.Strings.Left);
            when BSON_Int64 =>
               return Trim (Value.Int64_Value'Image, Ada.Strings.Left);
            when BSON_Double =>
               return Trim (Value.Double_Value'Image, Ada.Strings.Left);
            when BSON_Boolean =>
               return (if Value.Boolean_Value then "true" else "false");
            when BSON_Document =>
               return To_JSON (Value.Document_Value.all);
            when BSON_Array =>
               declare
                  Arr_Result : Unbounded_String := To_Unbounded_String ("[");
                  Arr_First  : Boolean := True;
               begin
                  for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                     if not Arr_First then
                        Append (Arr_Result, ", ");
                     end if;
                     Arr_First := False;

                     Append (Arr_Result, Format_Value (Value.Array_Value (I)));
                  end loop;

                  Append (Arr_Result, "]");
                  return To_String (Arr_Result);
               end;
            when BSON_Binary =>
               declare
                  Bin_Result : Unbounded_String := To_Unbounded_String ("{""$binary"":{""base64"":""");
                  -- Simplification: on devrait encoder en base64 ici
                  Append (Bin_Result, """");
                  Append (Bin_Result, ", ""subType"":""" &
                     Ada.Strings.Fixed.Trim (Value.Binary_Subtype'Image, Ada.Strings.Left) &
                     """}");
                  Append (Bin_Result, "}");
                  return To_String (Bin_Result);
               end;
            when BSON_ObjectId =>
               return "{""$oid"":""" & Value.ObjectId_Value & """}";
            when BSON_Date =>
               return "{""$date"":" &
                  Ada.Strings.Fixed.Trim (Value.Date_Value'Image, Ada.Strings.Left) &
                  "}";
            when BSON_Null =>
               return "null";
            when others =>
               return "null";
         end case;
      end Format_Value;
   begin
      if Doc.Is_Array then
         Result := To_Unbounded_String ("[");
      else
         Result := To_Unbounded_String ("{");
      end if;

      for C in Doc.Elements.Iterate loop
         if not First then
            Append (Result, ", ");
         end if;
         First := False;

         if not Doc.Is_Array then
            Append (Result, """" & To_String (Value_Maps.Key (C)) & """: ");
         end if;

         Append (Result, Format_Value (Value_Maps.Element (C)));
      end loop;

      if Doc.Is_Array then
         Append (Result, "]");
      else
         Append (Result, "}");
      end if;

      return To_String (Result);
   end To_JSON;

   procedure Write_Int32
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : Integer) is
      Int32_Value : constant Unsigned_32 := Unsigned_32 (Value);
   begin
      -- Format little-endian
      Buffer (Offset)     := Stream_Element (Int32_Value and 16#FF#);
      Buffer (Offset + 1) := Stream_Element ((Int32_Value / 16#100#) and 16#FF#);
      Buffer (Offset + 2) := Stream_Element ((Int32_Value / 16#10000#) and 16#FF#);
      Buffer (Offset + 3) := Stream_Element ((Int32_Value / 16#1000000#) and 16#FF#);
      Offset := Offset + 4;
   end Write_Int32;

   procedure Write_Int64
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : Long_Long_Integer) is
      Int64_Value : constant Unsigned_64 := Unsigned_64 (Value);
   begin
      -- Format little-endian
      for I in 0 .. 7 loop
         Buffer (Offset + Stream_Element_Offset (I)) :=
           Stream_Element ((Int64_Value / (2**(8*I))) and 16#FF#);
      end loop;
      Offset := Offset + 8;
   end Write_Int64;

   procedure Write_Double
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : Long_Float) is
      -- Simplification: conversion réelle requiert IEEE 754
      Int64_Value : constant Unsigned_64 := Unsigned_64
                                             (Long_Long_Integer (Value * 1000.0));
   begin
      -- Format little-endian
      for I in 0 .. 7 loop
         Buffer (Offset + Stream_Element_Offset (I)) :=
           Stream_Element ((Int64_Value / (2**(8*I))) and 16#FF#);
      end loop;
      Offset := Offset + 8;
   end Write_Double;

   procedure Write_String
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : String) is
      Length : constant Integer := Value'Length + 1; -- +1 pour le null-terminator
   begin
      -- Écrire la longueur (y compris le null-terminator)
      Write_Int32 (Buffer, Offset, Length);

      -- Écrire la chaîne
      for I in Value'Range loop
         Buffer (Offset) := Stream_Element (Character'Pos (Value (I)));
         Offset := Offset + 1;
      end loop;

      -- Écrire le null-terminator
      Buffer (Offset) := 0;
      Offset := Offset + 1;
   end Write_String;

   procedure Write_CString
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : String) is
   begin
      -- Écrire la chaîne
      for I in Value'Range loop
         Buffer (Offset) := Stream_Element (Character'Pos (Value (I)));
         Offset := Offset + 1;
      end loop;

      -- Écrire le null-terminator
      Buffer (Offset) := 0;
      Offset := Offset + 1;
   end Write_CString;

   procedure To_Binary
     (Doc    : in out BSON_Document_Type;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is

      -- Première passe: calculer la taille totale
      function Calculate_Size (D : BSON_Document_Type) return Integer is
         Total_Size : Integer := 5; -- 4 octets pour la taille + 1 octet pour le terminateur

         function Value_Size (Value : BSON_Value_Access) return Integer is
         begin
            case Value.Kind is
               when BSON_Double =>
                  return 8;
               when BSON_String =>
                  -- 4 octets pour la longueur + longueur de la chaîne + 1 octet pour le null-terminator
                  return 4 + Integer (Length (Value.String_Value)) + 1;
               when BSON_Document =>
                  return Calculate_Size (Value.Document_Value.all);
               when BSON_Array =>
                  declare
                     Array_Doc : BSON_Document_Type;
                     Array_Size : Integer := 5; -- Base document size
                  begin
                     for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                        declare
                           Index_Key : constant String :=
                             Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left);
                           Element : constant BSON_Value_Access := Value.Array_Value (I);
                        begin
                           -- 1 octet pour le type + longueur de la clé + 1 octet pour le null-terminator
                           Array_Size := Array_Size + 1 + Index_Key'Length + 1;
                           Array_Size := Array_Size + Value_Size (Element);
                        end;
                     end loop;

                     return Array_Size;
                  end;
               when BSON_Int32 =>
                  return 4;
               when BSON_Int64 =>
                  return 8;
               when BSON_Boolean =>
                  return 1;
               when BSON_Binary =>
                  -- 4 octets pour la longueur + 1 octet pour le sous-type + données binaires
                  return 4 + 1 + Integer (Value.Binary_Data.Length);
               when BSON_ObjectId =>
                  return 12; -- ObjectId est toujours 12 octets
               when BSON_Date =>
                  return 8; -- Date est un Int64
               when BSON_Null =>
                  return 0; -- Pas de données pour null
               when others =>
                  -- Types non implémentés
                  raise Program_Error with "Unsupported BSON type";
            end case;
         end Value_Size;
      begin
         for C in D.Elements.Iterate loop
            declare
               Key   : constant String := To_String (Value_Maps.Key (C));
               Value : constant BSON_Value_Access := Value_Maps.Element (C);
            begin
               -- 1 octet pour le type + longueur de la clé + 1 octet pour le null-terminator
               Total_Size := Total_Size + 1 + Key'Length + 1;

               -- Taille de la valeur
               Total_Size := Total_Size + Value_Size (Value);
            end;
         end loop;

         return Total_Size;
      end Calculate_Size;

      -- Deuxième passe: écrire les données dans le buffer
      procedure Write_Document
        (D : BSON_Document_Type;
         B : in out Stream_Element_Array;
         Offset : in out Stream_Element_Offset) is

         Start_Offset : constant Stream_Element_Offset := Offset;

         procedure Write_Value
           (Value : BSON_Value_Access;
            Key   : String) is
         begin
            -- Écrire le type
            case Value.Kind is
               when BSON_Double =>
                  B (Offset) := BSON_TYPE_DOUBLE;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  Write_Double (B, Offset, Value.Double_Value);

               when BSON_String =>
                  B (Offset) := BSON_TYPE_STRING;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  Write_String (B, Offset, To_String (Value.String_Value));

               when BSON_Document =>
                  B (Offset) := BSON_TYPE_DOCUMENT;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  Write_Document (Value.Document_Value.all, B, Offset);

               when BSON_Array =>
                  B (Offset) := BSON_TYPE_ARRAY;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

                  declare
                     Array_Start : constant Stream_Element_Offset := Offset;
                     Array_Size  : Integer;
                  begin
                     -- Réserver de l'espace pour la taille
                     Offset := Offset + 4;

                     -- Écrire les éléments du tableau
                     for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                        declare
                           Index_Key : constant String :=
                             Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left);
                           Element : constant BSON_Value_Access := Value.Array_Value (I);
                        begin
                           Write_Value (Element, Index_Key);
                        end;
                     end loop;

                     -- Écrire le terminateur
                     B (Offset) := 0;
                     Offset := Offset + 1;

                     -- Mettre à jour la taille
                     Array_Size := Integer (Offset - Array_Start);
                     Write_Int32 (B, Array_Start, Array_Size);
                  end;

               when BSON_Int32 =>
                  B (Offset) := BSON_TYPE_INT32;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  Write_Int32 (B, Offset, Value.Int32_Value);

               when BSON_Int64 =>
                  B (Offset) := BSON_TYPE_INT64;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  Write_Int64 (B, Offset, Value.Int64_Value);

               when BSON_Boolean =>
                  B (Offset) := BSON_TYPE_BOOLEAN;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  B (Offset) := (if Value.Boolean_Value then 1 else 0);
                  Offset := Offset + 1;

               when BSON_Binary =>
                  B (Offset) := BSON_TYPE_BINARY;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

                  -- Écrire la taille des données binaires
                  Write_Int32 (B, Offset, Integer (Value.Binary_Data.Length));

                  -- Écrire le sous-type
                  B (Offset) := Stream_Element (Value.Binary_Subtype);
                  Offset := Offset + 1;

                  -- Écrire les données binaires
                  for I in 0 .. Natural (Value.Binary_Data.Length) - 1 loop
                     B (Offset) := Value.Binary_Data (I);
                     Offset := Offset + 1;
                  end loop;

               when BSON_ObjectId =>
                  B (Offset) := BSON_TYPE_OBJECTID;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

                  -- Convertir l'ObjectId de hex à binaire
                  for I in 0 .. 11 loop
                     declare
                        Hex_Index : constant Integer := I * 2 + 1;
                        Hex_Substr : constant String := Value.ObjectId_Value (Hex_Index .. Hex_Index + 1);
                     begin
                        B (Offset) := Hex_To_Stream_Element (Hex_Substr);
                        Offset := Offset + 1;
                     end;
                  end loop;

               when BSON_Date =>
                  B (Offset) := BSON_TYPE_DATE;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  Write_Int64 (B, Offset, Value.Date_Value);

               when BSON_Null =>
                  B (Offset) := BSON_TYPE_NULL;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

               when others =>
                  raise Program_Error with "Unsupported BSON type";
            end case;
         end Write_Value;
      begin
         -- Réserver de l'espace pour la taille (4 octets)
         Offset := Offset + 4;

         -- Écrire tous les éléments
         for C in D.Elements.Iterate loop
            declare
               Key   : constant String := To_String (Value_Maps.Key (C));
               Value : constant BSON_Value_Access := Value_Maps.Element (C);
            begin
               Write_Value (Value, Key);
            end;
         end loop;

         -- Écrire le terminateur
         B (Offset) := 0;
         Offset := Offset + 1;

         -- Mettre à jour la taille
         declare
            Doc_Size : constant Integer := Integer (Offset - Start_Offset);
            Size_Offset : Stream_Element_Offset := Start_Offset;
         begin
            Write_Int32 (B, Size_Offset, Doc_Size);
         end;
      end Write_Document;

      Total_Size : constant Integer := Calculate_Size (Doc);
   begin
      if Total_Size > Buffer'Length then
         raise Constraint_Error with "Buffer too small";
      end if;

      Last := Buffer'First;
      Write_Document (Doc, Buffer, Last);
      Last := Last - 1;  -- Ajuster pour pointer vers le dernier élément écrit
   end To_Binary;

   function From_Binary (Buffer : Stream_Element_Array) return BSON_Document_Type is
      Doc    : BSON_Document_Type;
      Offset : Stream_Element_Offset := Buffer'First;

      function Read_Int32 return Integer is
         Result : Unsigned_32;
      begin
         -- Format little-endian
         Result := Unsigned_32 (Buffer (Offset)) +
                   Unsigned_32 (Buffer (Offset + 1)) * 16#100# +
                   Unsigned_32 (Buffer (Offset + 2)) * 16#10000# +
                   Unsigned_32 (Buffer (Offset + 3)) * 16#1000000#;
         Offset := Offset + 4;
         return Integer (Result);
      end Read_Int32;

      function Read_Int64 return Long_Long_Integer is
         Result : Unsigned_64 := 0;
      begin
         -- Format little-endian
         for I in 0 .. 7 loop
            Result := Result + Unsigned_64 (Buffer (Offset + Stream_Element_Offset (I))) *
                     (2**(8*I));
         end loop;
         Offset := Offset + 8;
         return Long_Long_Integer (Result);
      end Read_Int64;

      function Read_Double return Long_Float is
         -- Simplification: conversion réelle requiert IEEE 754
         Value : constant Long_Long_Integer := Read_Int64;
      begin
         return Long_Float (Value) / 1000.0;
      end Read_Double;

      function Read_CString return String is
         Start_Offset : constant Stream_Element_Offset := Offset;
         Length       : Stream_Element_Offset := 0;
      begin
         -- Trouver la fin de la chaîne
         while Offset <= Buffer'Last and then Buffer (Offset) /= 0 loop
            Offset := Offset + 1;
            Length := Length + 1;
         end loop;

         if Offset > Buffer'Last then
            raise Invalid_BSON_Format with "CString not null-terminated";
         end if;

         -- Avancer après le null-terminator
         Offset := Offset + 1;

         -- Convertir en String
         if Length = 0 then
            return "";
         else
            declare
               Result : String (1 .. Integer (Length));
            begin
               for I in 0 .. Length - 1 loop
                  Result (Integer (I + 1)) := Character'Val (Buffer (Start_Offset + I));
               end loop;
               return Result;
            end;
         end if;
      end Read_CString;

      function Read_String return String is
         Length : constant Integer := Read_Int32 - 1; -- -1 pour exclure le null-terminator
         Result : String (1 .. Length);
      begin
         for I in 1 .. Length loop
            Result (I) := Character'Val (Buffer (Offset));
            Offset := Offset + 1;
         end loop;

         -- Sauter le null-terminator
         Offset := Offset + 1;
         return Result;
      end Read_String;

      function Read_Document return BSON_Document_Type is
         Size        : constant Integer := Read_Int32;
         End_Offset  : constant Stream_Element_Offset := Offset + Stream_Element_Offset (Size - 4);
         Result      : BSON_Document_Type;
      begin
         Init_Document (Result);

         while Offset < End_Offset - 1 loop
            declare
               Element_Type : constant Stream_Element := Buffer (Offset);
               Key          : String;
            begin
               Offset := Offset + 1;
               Key := Read_CString;

               case Element_Type is
                  when BSON_TYPE_DOUBLE =>
                     Add_Double (Result, Key, Read_Double);

                  when BSON_TYPE_STRING =>
                     Add_String (Result, Key, Read_String);

                  when BSON_TYPE_DOCUMENT =>
                     declare
                        Sub_Doc : constant BSON_Document_Type := Read_Document;
                     begin
                        Add_Document (Result, Key, Sub_Doc);
                     end;

                  when BSON_TYPE_ARRAY =>
                     declare
                        Array_Doc : constant BSON_Document_Type := Read_Document;
                     begin
                        Add_Array (Result, Key);
                        -- Convertir les éléments indexés en éléments de tableau
                        for C in Array_Doc.Elements.Iterate loop
                           declare
                              Value : constant BSON_Value_Access := Value_Maps.Element (C);
                              Array_Value : BSON_Value_Type := Value.all;
                           begin
                              Array_Add_Value (Result, Key, Array_Value);
                           end;
                        end loop;
                     end;

                  when BSON_TYPE_BINARY =>
                     declare
                        Data_Length : constant Integer := Read_Int32;
                        Subtype_Val : constant BSON_Binary_Subtype := BSON_Binary_Subtype (Buffer (Offset));
                        Binary_Data : Stream_Element_Array (1 .. Stream_Element_Offset (Data_Length));
                     begin
                        Offset := Offset + 1; -- Sauter le sous-type
                        for I in Binary_Data'Range loop
                           Binary_Data (I) := Buffer (Offset);
                           Offset := Offset + 1;
                        end loop;
                        Add_Binary (Result, Key, Subtype_Val, Binary_Data);
                     end;

                  when BSON_TYPE_OBJECTID =>
                     declare
                        Oid : ObjectId_Type;
                        Hex_Chars : constant String := "0123456789ABCDEF";
                     begin
                        for I in 0 .. 11 loop
                           declare
                              B : constant Stream_Element := Buffer (Offset + Stream_Element_Offset (I));
                              High_Nibble : constant Integer := Integer (B) / 16;
                              Low_Nibble : constant Integer := Integer (B) mod 16;
                           begin
                              Oid (I * 2 + 1) := Hex_Chars (High_Nibble + 1);
                              Oid (I * 2 + 2) := Hex_Chars (Low_Nibble + 1);
                           end;
                        end loop;
                        Offset := Offset + 12;
                        Add_ObjectId (Result, Key, Oid);
                     end;

                  when BSON_TYPE_BOOLEAN =>
                     Add_Boolean (Result, Key, Buffer (Offset) /= 0);
                     Offset := Offset + 1;

                  when BSON_TYPE_DATE =>
                     Add_Date (Result, Key, Read_Int64);

                  when BSON_TYPE_NULL =>
                     declare
                        Null_Value : constant BSON_Value_Access :=
                          new BSON_Value_Type'(Kind => BSON_Null);
                     begin
                        Result.Elements.Insert (To_Unbounded_String (Key), Null_Value);
                     end;

                  when BSON_TYPE_INT32 =>
                     Add_Integer (Result, Key, Read_Int32);

                  when BSON_TYPE_INT64 =>
                     Add_Long_Integer (Result, Key, Read_Int64);

                  when others =>
                     raise Invalid_BSON_Format with "Unsupported BSON type: " &
                       Element_Type'Image;
               end case;
            end;
         end loop;

         -- Vérifier le terminateur
         if Buffer (Offset) /= 0 then
            raise Invalid_BSON_Format with "Document not properly terminated";
         end if;

         Offset := End_Offset;
         return Result;
      end Read_Document;
   begin
      Init_Document (Doc);

      -- Vérifier que le buffer a au moins 5 octets (taille + terminateur)
      if Buffer'Length < 5 then
         raise Invalid_BSON_Format with "Buffer too small";
      end if;

      Doc := Read_Document;
      return Doc;
   exception
      when E : others =>
         -- En cas d'erreur, retourner un document vide
         Free (Doc);
         Init_Document (Doc);
         return Doc;
   end From_Binary;

   function Is_Valid (Doc : BSON_Document_Type) return Boolean is
   begin
      -- Implémentation simple: vérifie que le document est initialisé
      return Doc.Elements /= Value_Maps.Empty_Map;
   end Is_Valid;

   procedure Validate (Doc : BSON_Document_Type) is
   begin
      if not Is_Valid (Doc) then
         raise BSON_Validation_Error with "Invalid BSON document";
      end if;
   end Validate;

   procedure Free_Value (Value : in out BSON_Value_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (BSON_Value_Type, BSON_Value_Access);
   begin
      if Value = null then
         return;
      end if;

      case Value.Kind is
         when BSON_Document =>
            if Value.Document_Value /= null then
               Free_Document (Value.Document_Value.all);
               declare
                  procedure Free_Doc is new Ada.Unchecked_Deallocation
                    (BSON_Document_Type, BSON_Document_Access);
               begin
                  Free_Doc (Value.Document_Value);
               end;
            end if;
         when BSON_Array =>
            -- Libérer chaque élément du tableau
            for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
               declare
                  Element : BSON_Value_Access := Value.Array_Value (I);
               begin
                  Free_Value (Element);
               end;
            end loop;
            Value.Array_Value.Clear;
         when BSON_Binary =>
            Value.Binary_Data.Clear;
         when others =>
            null; -- Pas de libération spéciale pour les autres types
      end case;

      Free (Value);
   end Free_Value;

   procedure Free_Document (Doc : in out BSON_Document_Type) is
   begin
      -- Libérer tous les éléments
      for C in Doc.Elements.Iterate loop
         declare
            Value : BSON_Value_Access := Value_Maps.Element (C);
         begin
            Free_Value (Value);
         end;
      end loop;

      -- Vider la map
      Doc.Elements.Clear;
   end Free_Document;

   procedure Free (Doc : in out BSON_Document_Type) is
   begin
      Free_Document (Doc);
   end Free;
end BSON;
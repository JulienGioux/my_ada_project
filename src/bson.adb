with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with BSON_Exceptions; use BSON_Exceptions;

package body BSON
  with SPARK_Mode => Off
is
   --  Constantes pour les types BSON
   BSON_TYPE_DOUBLE             : constant := 16#01#;
   BSON_TYPE_STRING             : constant := 16#02#;
   BSON_TYPE_DOCUMENT           : constant := 16#03#;
   BSON_TYPE_ARRAY              : constant := 16#04#;
   BSON_TYPE_BINARY             : constant := 16#05#;
   BSON_TYPE_UNDEFINED          : constant := 16#06#; --  Déprécié
   BSON_TYPE_OBJECTID           : constant := 16#07#;
   BSON_TYPE_BOOLEAN            : constant := 16#08#;
   BSON_TYPE_DATE               : constant := 16#09#;
   BSON_TYPE_NULL               : constant := 16#0A#;
   BSON_TYPE_REGEX              : constant := 16#0B#;
   BSON_TYPE_DBPOINTER          : constant := 16#0C#; --  Déprécié
   BSON_TYPE_JAVASCRIPT         : constant := 16#0D#;
   BSON_TYPE_SYMBOL             : constant := 16#0E#; --  Déprécié
   BSON_TYPE_JAVASCRIPT_W_SCOPE : constant := 16#0F#;
   BSON_TYPE_INT32              : constant := 16#10#;
   BSON_TYPE_TIMESTAMP          : constant := 16#11#;
   BSON_TYPE_INT64              : constant := 16#12#;
   BSON_TYPE_DECIMAL128         : constant := 16#13#;
   BSON_TYPE_MINKEY             : constant := 16#FF#;
   BSON_TYPE_MAXKEY             : constant := 16#7F#;

   procedure Init_Document (Doc : in out BSON_Document_Type) is
   begin
      Doc.Elements := Value_Maps.Empty_Map;
      Doc.Is_Array := False;
   end Init_Document;

   procedure Add_String
     (Doc : in out BSON_Document_Type; Key : String; Value : String)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind => BSON_String, String_Value => To_Unbounded_String (Value));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_String;

   procedure Add_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Integer)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Int32, Int32_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Integer;

   procedure Add_Long_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Long_Integer)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Int64, Int64_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Long_Integer;

   procedure Add_Double
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Float)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Double, Double_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Double;

   procedure Add_Boolean
     (Doc : in out BSON_Document_Type; Key : String; Value : Boolean)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Boolean, Boolean_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Boolean;

   procedure Add_Document
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : BSON_Document_Type)
   is
      New_Doc   : constant BSON_Document_Access :=
        new BSON_Document_Type'(Value);
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Document, Document_Value => New_Doc);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Document;

   procedure Add_Array (Doc : in out BSON_Document_Type; Key : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind => BSON_Array, Array_Value => BSON_Arrays.Empty_Vector);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Array;

   procedure Add_Binary
     (Doc           : in out BSON_Document_Type;
      Key           : String;
      Subtype_Value : BSON_Binary_Subtype;
      Data          : Stream_Element_Array)
   is
      New_Vector : Binary_Data_Vectors.Vector :=
        Binary_Data_Vectors.Empty_Vector;
      New_Value  : BSON_Value_Access;
   begin
      --  Copie des données binaires dans le vecteur
      for I in Data'Range loop
         New_Vector.Append (Data (I));
      end loop;

      New_Value :=
        new BSON_Value_Type'
          (Kind           => BSON_Binary,
           Binary_Subtype => Subtype_Value,
           Binary_Data    => New_Vector);

      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Binary;

   procedure Add_ObjectId
     (Doc : in out BSON_Document_Type; Key : String; Value : ObjectId_Type)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_ObjectId, ObjectId_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_ObjectId;

   procedure Add_Date
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Long_Integer)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Date, Date_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Date;

   procedure Add_Null (Doc : in out BSON_Document_Type; Key : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Null);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Null;

   procedure Add_Undefined (Doc : in out BSON_Document_Type; Key : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_Undefined);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Undefined;

   procedure Add_Regex
     (Doc     : in out BSON_Document_Type;
      Key     : String;
      Pattern : String;
      Options : String := "")
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind        => BSON_Regex,
           Regex_Value =>
             (Pattern => To_Unbounded_String (Pattern),
              Options => To_Unbounded_String (Options)));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Regex;

   procedure Add_JavaScript
     (Doc : in out BSON_Document_Type; Key : String; Code : String)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind             => BSON_JavaScript,
           JavaScript_Value => To_Unbounded_String (Code));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_JavaScript;

   procedure Add_Symbol
     (Doc : in out BSON_Document_Type; Key : String; Symbol : String)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind => BSON_Symbol, Symbol_Value => To_Unbounded_String (Symbol));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Symbol;

   procedure Add_JavaScript_W_Scope
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Code  : String;
      Scope : BSON_Document_Type)
   is
      New_Scope : constant BSON_Document_Access :=
        new BSON_Document_Type'(Scope);
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind           => BSON_JavaScript_W_Scope,
           JS_Scope_Value =>
             (Code => To_Unbounded_String (Code), Scope => New_Scope));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_JavaScript_W_Scope;

   procedure Add_Timestamp
     (Doc       : in out BSON_Document_Type;
      Key       : String;
      Seconds   : Unsigned_32;
      Increment : Unsigned_32)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind            => BSON_Timestamp,
           Timestamp_Value => (Increment => Increment, Seconds => Seconds));
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Timestamp;

   procedure Add_Decimal128
     (Doc : in out BSON_Document_Type; Key : String; Value : Decimal128_Type)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind => BSON_Decimal128, Decimal128_Value => Value);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_Decimal128;

   procedure Add_MinKey (Doc : in out BSON_Document_Type; Key : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_MinKey);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_MinKey;

   procedure Add_MaxKey (Doc : in out BSON_Document_Type; Key : String) is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'(Kind => BSON_MaxKey);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_MaxKey;

   procedure Add_DBPointer
     (Doc        : in out BSON_Document_Type;
      Key        : String;
      Collection : String;
      ObjectId   : ObjectId_Type)
   is
      New_Value : constant BSON_Value_Access :=
        new BSON_Value_Type'
          (Kind       => BSON_DBPointer,
           Collection => To_Unbounded_String (Collection),
           Pointer_Id => ObjectId);
   begin
      Doc.Elements.Insert (To_Unbounded_String (Key), New_Value);
   end Add_DBPointer;

   procedure Array_Add_Value
     (Doc : in out BSON_Document_Type; Key : String; Value : BSON_Value_Type)
   is
      Cursor       : Value_Maps.Cursor;
      Array_Access : BSON_Value_Access;
      New_Value    : BSON_Value_Access;
   begin
      Cursor := Doc.Elements.Find (To_Unbounded_String (Key));
      if not Value_Maps.Has_Element (Cursor) then
         raise BSON_Key_NotFound
           with "Key '" & Key & "' not found in document";
      end if;

      Array_Access := Value_Maps.Element (Cursor);
      if Array_Access.Kind /= BSON_Array then
         raise BSON_Type_Mismatch
           with "Element with key '" & Key & "' is not an array";
      end if;

      --  Création d'une copie de la valeur
      New_Value := new BSON_Value_Type'(Value);

      Array_Access.Array_Value.Append (New_Value);
   end Array_Add_Value;

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
               --  Simplified string construction to avoid syntax issues
               return
                 "{""$binary"":{""base64"":"""", ""subType"":"""
                 & Trim (Value.Binary_Subtype'Image, Ada.Strings.Left)
                 & """}}";

            when BSON_ObjectId =>
               return "{""$oid"":""" & Value.ObjectId_Value & """}";

            when BSON_Date =>
               return
                 "{""$date"":"
                 & Trim (Value.Date_Value'Image, Ada.Strings.Left)
                 & "}";

            when BSON_Null =>
               return "null";

            when BSON_Undefined =>
               return "{""$undefined"":true}";

            when BSON_Regex =>
               return
                 "{""$regex"":"""
                 & To_String (Value.Regex_Value.Pattern)
                 & """, ""$options"":"""
                 & To_String (Value.Regex_Value.Options)
                 & """}";

            when BSON_DBPointer =>
               return
                 "{""$dbPointer"":{""$ref"":"""
                 & To_String (Value.Collection)
                 & """, ""$id"":{""$oid"":"""
                 & Value.Pointer_Id
                 & """}}}";

            when BSON_JavaScript =>
               return
                 "{""$code"":""" & To_String (Value.JavaScript_Value) & """}";

            when BSON_Symbol =>
               return
                 "{""$symbol"":""" & To_String (Value.Symbol_Value) & """}";

            when BSON_JavaScript_W_Scope =>
               return
                 "{""$code"":"""
                 & To_String (Value.JS_Scope_Value.Code)
                 & """, ""$scope"":"
                 & To_JSON (Value.JS_Scope_Value.Scope.all)
                 & "}";

            when BSON_Timestamp =>
               declare
                  Seconds_Str : constant String :=
                    Trim
                      (Value.Timestamp_Value.Seconds'Image, Ada.Strings.Left);
                  Incr_Str    : constant String :=
                    Trim
                      (Value.Timestamp_Value.Increment'Image,
                       Ada.Strings.Left);
               begin
                  return
                    "{""$timestamp"":{""t"":"
                    & Seconds_Str
                    & ", ""i"":"
                    & Incr_Str
                    & "}}";
               end;

            when BSON_Decimal128 =>
               --  Simplification - conversion des données
               --  binaires en JSON simplifiée
               return "{""$numberDecimal"":""0""}";  --  Valeur par défaut

            when BSON_MinKey =>
               return "{""$minKey"":1}";

            when BSON_MaxKey =>
               return "{""$maxKey"":1}";
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
      Value  : Integer)
   is
      Int32_Value     : constant Unsigned_32 := Unsigned_32 (Value);
      Available_bytes : constant Integer := Integer (Buffer'Last - Offset + 1);
   begin
      --  Vérifier qu'il y a assez d'espace pour écrire 4 octets
      if Offset + 3 > Buffer'Last then
         raise BSON_Buffer_Overflow
           with
             "Not enough space to write Int32. Required: 4 bytes, Available: "
             & Available_bytes'Image;
      end if;

      --  Format little-endian
      Buffer (Offset) := Stream_Element (Int32_Value and 16#FF#);
      Buffer (Offset + 1) :=
        Stream_Element ((Int32_Value / 16#100#) and 16#FF#);
      Buffer (Offset + 2) :=
        Stream_Element ((Int32_Value / 16#1_0000#) and 16#FF#);
      Buffer (Offset + 3) :=
        Stream_Element ((Int32_Value / 16#100_0000#) and 16#FF#);
      Offset := Offset + 4;
   end Write_Int32;

   procedure Write_Int64
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : Long_Long_Integer)
   is
      Int64_Value     : constant Unsigned_64 := Unsigned_64 (Value);
      Available_bytes : constant Integer := Integer (Buffer'Last - Offset + 1);
   begin
      --  Vérifier qu'il y a assez d'espace pour écrire 8 octets
      if Offset + 7 > Buffer'Last then
         raise BSON_Buffer_Overflow
           with
             "Not enough space to write Int64. Required: 8 bytes, Available: "
             & Available_bytes'Image;
      end if;

      --  Format little-endian
      for I in 0 .. 7 loop
         Buffer (Offset + Stream_Element_Offset (I)) :=
           Stream_Element ((Int64_Value / (2**(8 * I))) and 16#FF#);
      end loop;
      Offset := Offset + 8;
   end Write_Int64;

   procedure Write_Double
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : Long_Float)
   is
      --  Conversion IEEE 754 pour les doubles
      function To_U64 is new
        Ada.Unchecked_Conversion (Long_Float, Unsigned_64);
      Raw_Bits : Unsigned_64;
   begin
      --  Gestion des cas spéciaux selon la norme IEEE 754
      if Value = 0.0 then
         Raw_Bits := 0;  --  +0.0 en IEEE 754
      elsif not Value'Valid then
         Raw_Bits := 16#7FF8_0000_0000_0000#;
         --  NaN (Not-a-Number)

      elsif Value > 0.0 and then Value > Long_Float'Last then
         Raw_Bits := 16#7FF0_0000_0000_0000#;  --  +Infinity
      elsif Value < 0.0 and then Value < Long_Float'First then
         Raw_Bits := 16#FFF0_0000_0000_0000#;  --  -Infinity
      else
         --  Valeur normale
         Raw_Bits := To_U64 (Value);
      end if;

      --  Format little-endian (octets inversés pour BSON)
      for I in 0 .. 7 loop
         Buffer (Offset + Stream_Element_Offset (I)) :=
           Stream_Element ((Raw_Bits / (2**(8 * I))) and 16#FF#);
      end loop;
      Offset := Offset + 8;
   end Write_Double;

   procedure Write_String
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : String)
   is
      Length          : constant Integer :=
        Value'Length + 1; --  +1 pour le null-terminator
      Required_Length : constant Integer :=
        Integer (Stream_Element_Offset'(4)) + Length;
      Available_bytes : constant Integer := Integer (Buffer'Last - Offset + 1);
   begin
      --  Vérifier qu'il y a assez d'espace pour la longueur (4 octets),
      --  la chaîne et le null-terminator
      if Offset + Stream_Element_Offset (Length) + 3 > Buffer'Last then
         raise BSON_Buffer_Overflow
           with
             "Not enough space to write string. Required: "
             & Required_Length'Image
             & " bytes, Available: "
             & Available_bytes'Image;
      end if;

      --  Écrire la longueur (y compris le null-terminator)
      Write_Int32 (Buffer, Offset, Length);

      --  Écrire la chaîne
      for I in Value'Range loop
         Buffer (Offset) := Stream_Element (Character'Pos (Value (I)));
         Offset := Offset + 1;
      end loop;

      --  Écrire le null-terminator
      Buffer (Offset) := 0;
      Offset := Offset + 1;
   end Write_String;

   procedure Write_CString
     (Buffer : in out Stream_Element_Array;
      Offset : in out Stream_Element_Offset;
      Value  : String) is
   begin
      --  Écrire la chaîne
      for I in Value'Range loop
         Buffer (Offset) := Stream_Element (Character'Pos (Value (I)));
         Offset := Offset + 1;
      end loop;

      --  Écrire le null-terminator
      Buffer (Offset) := 0;
      Offset := Offset + 1;
   end Write_CString;

   procedure To_Binary
     (Doc    : in out BSON_Document_Type;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      --  Variable pour stocker la taille totale calculée
      Doc_Total_Size : Integer;

      --  Première passe: calculer la taille totale
      function Calculate_Size (D : BSON_Document_Type) return Integer is
         Total_Size : Integer :=
           5; --  4 octets pour la taille + 1 octet pour le terminateur

         function Value_Size (Value : BSON_Value_Access) return Integer is
         begin
            case Value.Kind is
               when BSON_Double =>
                  return 8;

               when BSON_String =>
                  --  4 octets pour la longueur + longueur de la chaîne
                  --  + 1 octet pour le null-terminator
                  return 4 + Integer (Length (Value.String_Value)) + 1;

               when BSON_Document =>
                  return Calculate_Size (Value.Document_Value.all);

               when BSON_Array =>
                  declare
                     Array_Size : Integer := 5; --  Base document size
                  begin
                     for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                        Array_Size := Array_Size + 1; --  Type byte
                        Array_Size :=
                          Array_Size
                          + Integer'Image (I)'Length
                          + 1; --  Key (index) + null terminator
                        Array_Size :=
                          Array_Size + Value_Size (Value.Array_Value (I));
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
                  --  4 octets pour la longueur + 1 octet
                  --  pour le sous-type + données binaires
                  declare
                     Bin_Length : constant Natural :=
                       Natural (Value.Binary_Data.Length);
                  begin
                     return 4 + 1 + Bin_Length;
                  end;

               when BSON_ObjectId =>
                  return 12;
                  --  ObjectId est toujours 12 octets

               when BSON_Date =>
                  return 8; --  Date est un Int64

               when BSON_Null =>
                  return 0; --  Pas de données pour null

               when BSON_Undefined =>
                  return 0; --  Pas de données pour undefined

               when BSON_Regex =>
                  --  Deux C-strings (pattern et options)
                  return
                    Integer (Length (Value.Regex_Value.Pattern))
                    + 1
                    + Integer (Length (Value.Regex_Value.Options))
                    + 1;

               when BSON_JavaScript =>
                  --  String (4 octets taille + string + 1 octet null)
                  return 4 + Integer (Length (Value.JavaScript_Value)) + 1;

               when BSON_Symbol =>
                  --  String (4 octets taille + string + 1 octet null)
                  return 4 + Integer (Length (Value.Symbol_Value)) + 1;

               when BSON_JavaScript_W_Scope =>
                  --  4 octets taille totale + 4 octets taille string
                  --  + string + 1 octet null + document scope
                  return
                    4
                    + 4
                    + Integer (Length (Value.JS_Scope_Value.Code))
                    + 1
                    + Calculate_Size (Value.JS_Scope_Value.Scope.all);

               when BSON_DBPointer =>
                  --  String (4 octets taille + string + 1 octet null)
                  --  + ObjectId (12 octets)
                  return 4 + Integer (Length (Value.Collection)) + 1 + 12;

               when BSON_Timestamp =>
                  return 8; --  Deux Int32 (increment & seconds)

               when BSON_Decimal128 =>
                  return 16; --  128 bits (16 octets)

               when BSON_MinKey | BSON_MaxKey =>
                  return 0;
                  --  Pas de données pour MinKey/MaxKey
            end case;
         end Value_Size;
      begin
         for C in D.Elements.Iterate loop
            declare
               Key   : constant String := To_String (Value_Maps.Key (C));
               Value : constant BSON_Value_Access := Value_Maps.Element (C);
            begin
               --  1 octet pour le type + longueur de la clé
               --  + 1 octet pour le null-terminator
               Total_Size := Total_Size + 1 + Key'Length + 1;

               --  Taille de la valeur
               Total_Size := Total_Size + Value_Size (Value);
            end;
         end loop;

         return Total_Size;
      end Calculate_Size;

      --  Deuxième passe: écrire les données dans le buffer
      procedure Write_Document
        (D      : BSON_Document_Type;
         B      : in out Stream_Element_Array;
         Offset : in out Stream_Element_Offset)
      is
         Start_Offset : constant Stream_Element_Offset := Offset;
         Temp_Offset  : Stream_Element_Offset;

         procedure Write_Value (Value : BSON_Value_Access; Key : String) is
         begin
            --  Écrire le type
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
                     Temp_Offset : Stream_Element_Offset := Array_Start;
                     Array_Size  : Integer;
                  begin
                     --  Réserver de l'espace pour la taille
                     Offset := Offset + 4;
                     --  Écrire les éléments du tableau
                     for I in 0 .. Natural (Value.Array_Value.Length) - 1 loop
                        --  Utiliser l'indice comme clé
                        Write_Value (Value.Array_Value (I), Integer'Image (I));
                     end loop;
                     --  Écrire le terminateur
                     B (Offset) := 0;
                     Offset := Offset + 1;
                     --  Mettre à jour la taille
                     Array_Size := Integer (Offset - Array_Start);
                     Write_Int32 (B, Temp_Offset, Array_Size);
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

                  declare
                     Bin_Length : constant Integer :=
                       Integer (Value.Binary_Data.Length);
                  begin
                     --  Écrire la longueur
                     Write_Int32 (B, Offset, Bin_Length);

                     --  Écrire le sous-type
                     B (Offset) := Stream_Element (Value.Binary_Subtype);
                     Offset := Offset + 1;

                     --  Écrire les données binaires
                     for I in 0 .. Bin_Length - 1 loop
                        B (Offset) := Value.Binary_Data (I);
                        Offset := Offset + 1;
                     end loop;
                  end;

               when BSON_ObjectId =>
                  B (Offset) := BSON_TYPE_OBJECTID;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire l'ObjectId (12 octets)
                  declare
                     Hex_Value  : Unsigned_8;
                     Chr1, Chr2 : Character;
                  begin
                     for I in 1 .. 12 loop
                        Chr1 := Value.ObjectId_Value (2 * I - 1);
                        Chr2 := Value.ObjectId_Value (2 * I);

                        --  Convertir deux caractères hex en un octet
                        if Chr1 in '0' .. '9' then
                           Hex_Value :=
                             Character'Pos (Chr1) - Character'Pos ('0');
                        else
                           Hex_Value :=
                             Character'Pos (Chr1) - Character'Pos ('A') + 10;
                        end if;

                        Hex_Value := Hex_Value * 16;

                        if Chr2 in '0' .. '9' then
                           Hex_Value :=
                             Hex_Value
                             + (Character'Pos (Chr2) - Character'Pos ('0'));
                        else
                           Hex_Value :=
                             Hex_Value
                             + (Character'Pos (Chr2)
                                - Character'Pos ('A')
                                + 10);
                        end if;

                        B (Offset) := Stream_Element (Hex_Value);
                        Offset := Offset + 1;
                     end loop;
                  end;

               when BSON_Date =>
                  B (Offset) := BSON_TYPE_DATE;
                  Offset := Offset + 1;

                  Write_CString (B, Offset, Key);
                  Write_Int64 (B, Offset, Value.Date_Value);

               when BSON_Null =>
                  B (Offset) := BSON_TYPE_NULL;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

               when BSON_Undefined =>
                  B (Offset) := BSON_TYPE_UNDEFINED;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

               when BSON_Regex =>
                  B (Offset) := BSON_TYPE_REGEX;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire le motif regex comme CString
                  Write_CString
                    (B, Offset, To_String (Value.Regex_Value.Pattern));
                  --  Écrire les options regex comme CString
                  Write_CString
                    (B, Offset, To_String (Value.Regex_Value.Options));

               when BSON_DBPointer =>
                  B (Offset) := BSON_TYPE_DBPOINTER;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire la collection comme String
                  Write_String (B, Offset, To_String (Value.Collection));
                  --  Écrire l'ObjectId (12 octets)
                  declare
                     Hex_Value  : Unsigned_8;
                     Chr1, Chr2 : Character;
                  begin
                     for I in 1 .. 12 loop
                        Chr1 := Value.Pointer_Id (2 * I - 1);
                        Chr2 := Value.Pointer_Id (2 * I);

                        --  Convertir deux caractères hex en un octet
                        if Chr1 in '0' .. '9' then
                           Hex_Value :=
                             Character'Pos (Chr1) - Character'Pos ('0');
                        else
                           Hex_Value :=
                             Character'Pos (Chr1) - Character'Pos ('A') + 10;
                        end if;

                        Hex_Value := Hex_Value * 16;

                        if Chr2 in '0' .. '9' then
                           Hex_Value :=
                             Hex_Value
                             + (Character'Pos (Chr2) - Character'Pos ('0'));
                        else
                           Hex_Value :=
                             Hex_Value
                             + (Character'Pos (Chr2)
                                - Character'Pos ('A')
                                + 10);
                        end if;

                        B (Offset) := Stream_Element (Hex_Value);
                        Offset := Offset + 1;
                     end loop;
                  end;

               when BSON_JavaScript =>
                  B (Offset) := BSON_TYPE_JAVASCRIPT;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire le code JavaScript comme String
                  Write_String (B, Offset, To_String (Value.JavaScript_Value));

               when BSON_Symbol =>
                  B (Offset) := BSON_TYPE_SYMBOL;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire le symbole comme String
                  Write_String (B, Offset, To_String (Value.Symbol_Value));

               when BSON_JavaScript_W_Scope =>
                  B (Offset) := BSON_TYPE_JAVASCRIPT_W_SCOPE;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

                  declare
                     Start_Total : constant Stream_Element_Offset := Offset;
                     Temp_Total  : Stream_Element_Offset := Start_Total;
                     Total_Size  : Integer;
                  begin
                     --  Réserver de l'espace pour la taille totale
                     Offset := Offset + 4;

                     --  Écrire le code JavaScript comme String
                     Write_String
                       (B, Offset, To_String (Value.JS_Scope_Value.Code));

                     --  Écrire le document scope
                     Write_Document
                       (Value.JS_Scope_Value.Scope.all, B, Offset);

                     --  Mettre à jour la taille totale
                     Total_Size := Integer (Offset - Start_Total);
                     Write_Int32 (B, Temp_Total, Total_Size);
                  end;

               when BSON_Timestamp =>
                  B (Offset) := BSON_TYPE_TIMESTAMP;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire l'increment (4 octets)
                  declare
                     Inc_Value : constant Integer :=
                       Integer (Value.Timestamp_Value.Increment);
                  begin
                     Write_Int32 (B, Offset, Inc_Value);
                  end;
                  --  Écrire les secondes (4 octets)
                  declare
                     Sec_Value : constant Integer :=
                       Integer (Value.Timestamp_Value.Seconds);
                  begin
                     Write_Int32 (B, Offset, Sec_Value);
                  end;

               when BSON_Decimal128 =>
                  B (Offset) := BSON_TYPE_DECIMAL128;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
                  --  Écrire les 16 octets de données Decimal128
                  for I in Value.Decimal128_Value'Range loop
                     B (Offset) := Value.Decimal128_Value (I);
                     Offset := Offset + 1;
                  end loop;

               when BSON_MinKey =>
                  B (Offset) := BSON_TYPE_MINKEY;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);

               when BSON_MaxKey =>
                  B (Offset) := BSON_TYPE_MAXKEY;
                  Offset := Offset + 1;
                  Write_CString (B, Offset, Key);
            end case;
         end Write_Value;
      begin
         --  Réserver de l'espace pour la taille (4 octets)
         Offset := Offset + 4;

         --  Écrire tous les éléments
         for C in D.Elements.Iterate loop
            declare
               Key   : constant String := To_String (Value_Maps.Key (C));
               Value : constant BSON_Value_Access := Value_Maps.Element (C);
            begin
               Write_Value (Value, Key);
            end;
         end loop;

         --  Écrire le terminateur
         B (Offset) := 0;
         Offset := Offset + 1;

         --  Mettre à jour la taille
         declare
            Doc_Size    : constant Integer := Integer (Offset - Start_Offset);
            Size_Offset : Stream_Element_Offset := Start_Offset;
         begin
            Write_Int32 (B, Size_Offset, Doc_Size);
         end;
      end Write_Document;

   begin
      --  Calculer d'abord la taille totale requise
      Doc_Total_Size := Calculate_Size (Doc);

      --  Vérifier si le buffer est suffisamment grand
      if Doc_Total_Size > Buffer'Length then
         raise BSON_Buffer_Overflow
           with
             "Buffer too small. Required size: "
             & Doc_Total_Size'Image
             & ", Buffer size: "
             & Buffer'Length'Image;
      end if;

      Last := Buffer'First;
      Write_Document (Doc, Buffer, Last);
      Last := Last - 1;
      --  Ajuster pour pointer vers le dernier élément écrit
   end To_Binary;

   function From_Binary
     (Buffer : Stream_Element_Array) return BSON_Document_Type
   is
      Doc    : BSON_Document_Type;
      Offset : Stream_Element_Offset := Buffer'First;

      function Read_Int32 return Integer is
         Result : Unsigned_32;
      begin
         --  Format little-endian
         Result :=
           Unsigned_32 (Buffer (Offset))
           + Unsigned_32 (Buffer (Offset + 1)) * 16#100#
           + Unsigned_32 (Buffer (Offset + 2)) * 16#1_0000#
           + Unsigned_32 (Buffer (Offset + 3)) * 16#100_0000#;
         Offset := Offset + 4;
         return Integer (Result);
      end Read_Int32;

      function Read_Int64 return Long_Long_Integer is
         Result : Unsigned_64 := 0;
      begin
         --  Format little-endian
         for I in 0 .. 7 loop
            Result :=
              Result
              + Unsigned_64 (Buffer (Offset + Stream_Element_Offset (I)))
                * (2**(8 * I));
         end loop;
         Offset := Offset + 8;
         return Long_Long_Integer (Result);
      end Read_Int64;

      function Read_Double return Long_Float is
         Raw_Bits         : Unsigned_64 := 0;
         function To_Float is new
           Ada.Unchecked_Conversion (Unsigned_64, Long_Float);
         function To_Long_Float is new
           Ada.Unchecked_Conversion (Unsigned_64, Long_Float);
         --  Constantes pour les valeurs IEEE 754 spéciales
         POS_INF          : constant Unsigned_64 := 16#7FF0_0000_0000_0000#;
         NEG_INF          : constant Unsigned_64 := 16#FFF0_0000_0000_0000#;
         NAN_MASK         : constant Unsigned_64 := 16#7FF0_0000_0000_0000#;
         NAN_PAYLOAD_MASK : constant Unsigned_64 := 16#000F_FFFF_FFFF_FFFF#;
         NAN_VALUE        : constant Unsigned_64 := 16#7FF8_0000_0000_0000#;
         POS_ZERO         : constant Unsigned_64 := 0;
         NEG_ZERO         : constant Unsigned_64 := 16#8000_0000_0000_0000#;

         --  Variable pour stocker le résultat
         Result : Long_Float;
      begin
         --  Format little-endian
         for I in 0 .. 7 loop
            Raw_Bits :=
              Raw_Bits
              + (Unsigned_64 (Buffer (Offset + Stream_Element_Offset (I)))
                 * (2**(8 * I)));
         end loop;
         Offset := Offset + 8;

         --  Traitement des valeurs spéciales
         --  en utilisant une approche sans division
         if Raw_Bits = POS_ZERO then
            Result := 0.0;  --  +0.0
         elsif Raw_Bits = NEG_ZERO then
            Result := -0.0; --  -0.0
         elsif Raw_Bits = POS_INF then
            --  +Infinity: utiliser la valeur la plus grande possible
            Result := Long_Float'Last;
         elsif Raw_Bits = NEG_INF then
            --  -Infinity: utiliser la valeur la plus petite possible
            Result := Long_Float'First;
         elsif (Raw_Bits and NAN_MASK) = NAN_MASK
           and then (Raw_Bits and NAN_PAYLOAD_MASK) /= 0
         then
            --  NaN: utiliser une valeur non définie
            Result := To_Long_Float (NAN_VALUE);
         else
            --  Valeur normale
            Result := To_Float (Raw_Bits);
         end if;

         return Result;
      end Read_Double;

      function Read_CString return String is
         Start_Offset : constant Stream_Element_Offset := Offset;
         Length       : Stream_Element_Offset := 0;
      begin
         --  Trouver la fin de la chaîne
         while Offset <= Buffer'Last and then Buffer (Offset) /= 0 loop
            Offset := Offset + 1;
            Length := Length + 1;
         end loop;

         if Offset > Buffer'Last then
            raise Invalid_BSON_Format with "CString not null-terminated";
         end if;

         --  Avancer après le null-terminator
         Offset := Offset + 1;

         --  Convertir en String
         if Length = 0 then
            return "";
         else
            declare
               Result : String (1 .. Integer (Length));
            begin
               for I in 0 .. Length - 1 loop
                  Result (Integer (I + 1)) :=
                    Character'Val (Buffer (Start_Offset + I));
               end loop;
               return Result;
            end;
         end if;
      end Read_CString;

      function Read_String return String is
         Length : constant Integer :=
           Read_Int32 - 1; --  -1 pour exclure le null-terminator
         Result : String (1 .. Length);
      begin
         for I in 1 .. Length loop
            Result (I) := Character'Val (Buffer (Offset));
            Offset := Offset + 1;
         end loop;

         --  Sauter le null-terminator
         Offset := Offset + 1;
         return Result;
      end Read_String;

      function Read_Binary return Binary_Data_Vectors.Vector is
         Length : constant Integer := Read_Int32;
         --  Subtype_Val non utilisé, supprimé pour éliminer l'avertissement
         Result : Binary_Data_Vectors.Vector :=
           Binary_Data_Vectors.Empty_Vector;
      begin
         Offset := Offset + 1;  --  Skip subtype byte
         for I in 1 .. Length loop
            Result.Append (Buffer (Offset));
            Offset := Offset + 1;
         end loop;
         return Result;
      end Read_Binary;

      function Read_ObjectId return ObjectId_Type is
         Hex_Chars  : constant String := "0123456789ABCDEF";
         Result     : ObjectId_Type := (others => ' ');
         Idx        : Positive := 1;
         Byte_Value : Stream_Element;
      begin
         for I in 1 .. 12 loop
            Byte_Value := Buffer (Offset);
            --  Convertir chaque octet en deux caractères hexadécimaux
            Result (Idx) := Hex_Chars (Integer (Byte_Value / 16) + 1);
            Result (Idx + 1) := Hex_Chars (Integer (Byte_Value mod 16) + 1);
            Idx := Idx + 2;
            Offset := Offset + 1;
         end loop;
         return Result;
      end Read_ObjectId;

      function Read_Document return BSON_Document_Type is
         Size       : constant Integer := Read_Int32;
         End_Offset : constant Stream_Element_Offset :=
           Offset + Stream_Element_Offset (Size - 4);
         Result     : BSON_Document_Type;
      begin
         Init_Document (Result);

         while Offset < End_Offset - 1 loop
            declare
               Element_Type : constant Stream_Element := Buffer (Offset);
               Key          : constant String := Read_CString;
               New_Value    : BSON_Value_Access;
            begin
               Offset := Offset + 1;
               case Element_Type is
                  when BSON_TYPE_DOUBLE =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind => BSON_Double, Double_Value => Read_Double);
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_STRING =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind         => BSON_String,
                          String_Value => To_Unbounded_String (Read_String));
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_DOCUMENT =>
                     declare
                        Doc_Value  : constant BSON_Document_Type :=
                          Read_Document;
                        Doc_Access : constant BSON_Document_Access :=
                          new BSON_Document_Type'(Doc_Value);
                     begin
                        New_Value :=
                          new BSON_Value_Type'
                            (Kind           => BSON_Document,
                             Document_Value => Doc_Access);
                        Result.Elements.Insert
                          (To_Unbounded_String (Key), New_Value);
                     end;

                  when BSON_TYPE_ARRAY =>
                     declare
                        Skip_Size : Integer;
                        Skip_End  : Stream_Element_Offset;
                        V         : BSON_Arrays.Vector;
                     begin
                        --  Lire la taille et calculer où nous devons sauter
                        Skip_Size := Read_Int32;
                        Skip_End :=
                          Offset + Stream_Element_Offset (Skip_Size - 4);

                        --  Créer un vecteur vide initialisé
                        V := BSON_Arrays.Empty_Vector;

                        --  Créer la valeur avec ce vecteur
                        New_Value :=
                          new BSON_Value_Type'
                            (Kind => BSON_Array, Array_Value => V);

                        --  Sauter le contenu du tableau
                        Offset := Skip_End;

                        --  Ajouter au document
                        Result.Elements.Insert
                          (To_Unbounded_String (Key), New_Value);
                     end;

                  when BSON_TYPE_BINARY =>
                     declare
                        Bin_Data    : constant Binary_Data_Vectors.Vector :=
                          Read_Binary;
                        Bin_Subtype : constant BSON_Binary_Subtype :=
                          0;  --  Default
                     begin
                        New_Value :=
                          new BSON_Value_Type'
                            (Kind           => BSON_Binary,
                             Binary_Subtype => Bin_Subtype,
                             Binary_Data    => Bin_Data);
                        Result.Elements.Insert
                          (To_Unbounded_String (Key), New_Value);
                     end;

                  when BSON_TYPE_OBJECTID =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind           => BSON_ObjectId,
                          ObjectId_Value => Read_ObjectId);
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_BOOLEAN =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind          => BSON_Boolean,
                          Boolean_Value => (Buffer (Offset) /= 0));
                     Offset := Offset + 1;
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_DATE =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind => BSON_Date, Date_Value => Read_Int64);
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_NULL =>
                     New_Value := new BSON_Value_Type'(Kind => BSON_Null);
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_INT32 =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind => BSON_Int32, Int32_Value => Read_Int32);
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when BSON_TYPE_INT64 =>
                     New_Value :=
                       new BSON_Value_Type'
                         (Kind => BSON_Int64, Int64_Value => Read_Int64);
                     Result.Elements.Insert
                       (To_Unbounded_String (Key), New_Value);

                  when others =>
                     --  Skip unknown types for now
                     null;
               end case;
            end;
         end loop;

         --  Vérifier le terminateur
         if Buffer (Offset) /= 0 then
            raise Invalid_BSON_Format with "Document not properly terminated";
         end if;

         Offset := End_Offset;
         return Result;
      end Read_Document;
   begin
      Init_Document (Doc);

      --  Vérifier que le buffer a au moins 5 octets (taille + terminateur)
      if Buffer'Length < 5 then
         raise BSON_Invalid_Format
           with
             "Buffer too small. Minimum size: 5, Actual size: "
             & Buffer'Length'Image;
      end if;

      Doc := Read_Document;
      return Doc;
   exception
      when others =>
         --  En cas d'erreur, retourner un document vide
         Free (Doc);
         Init_Document (Doc);
         return Doc;
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

                        Element : constant BSON_Value_Access :=
                          Value.Array_Value (I);
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

   procedure Free_Value (Value : in out BSON_Value_Access) is
      procedure Free is new
        Ada.Unchecked_Deallocation (BSON_Value_Type, BSON_Value_Access);
   begin
      if Value = null then
         return;
      end if;

      case Value.Kind is
         when BSON_Document =>
            if Value.Document_Value /= null then
               Free_Document (Value.Document_Value.all);
               declare
                  procedure Free_Doc is new
                    Ada.Unchecked_Deallocation
                      (BSON_Document_Type,
                       BSON_Document_Access);
               begin
                  Free_Doc (Value.Document_Value);
               end;
            end if;

         when BSON_Array =>
            --  Libérer chaque élément du tableau
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
            --  Pas de libération spéciale pour les autres types
            null;
      end case;

      Free (Value);
   end Free_Value;

   procedure Free_Document (Doc : in out BSON_Document_Type) is
      procedure Free is new
        Ada.Unchecked_Deallocation (BSON_Value_Type, BSON_Value_Access);
      Temp : BSON_Value_Access;
   begin
      for C in Doc.Elements.Iterate loop
         Temp := Value_Maps.Element (C);
         Free_Value (Temp);
         Free (Temp);
      end loop;
      Doc.Elements.Clear;
   end Free_Document;

   procedure Free (Doc : in out BSON_Document_Type) is
   begin
      Free_Document (Doc);
   end Free;
end BSON;

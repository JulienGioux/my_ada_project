with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Streams;           use Ada.Streams;
with Interfaces;            use Interfaces;

package BSON
  with SPARK_Mode => Off
is
   --  Sous-types pour la validation
   subtype Valid_Buffer_Size is Integer range 5 .. Integer'Last;
   subtype Valid_Array_Index is Integer range 0 .. Integer'Last;
   subtype Valid_String_Length is Integer range 0 .. Integer'Last;
   subtype Valid_Document_Size is Stream_Element_Offset range 5 .. Stream_Element_Offset'Last;

   --  Types
   type BSON_Type is
     (BSON_Double,
      BSON_String,
      BSON_Document,
      BSON_Array,
      BSON_Binary,
      BSON_ObjectId,
      BSON_Boolean,
      BSON_Date,
      BSON_Null,
      BSON_Int32,
      BSON_Int64,
      BSON_Undefined,     --  Type déprécié (0x06)
      BSON_Regex,         --  Expression régulière (0x0B)
      BSON_DBPointer,     --  Type déprécié (0x0C)
      BSON_JavaScript,    --  Code JavaScript (0x0D)
      BSON_Symbol,        --  Symbole
      BSON_JavaScript_W_Scope,  --  JavaScript avec scope
      BSON_Timestamp,     --  Timestamp interne MongoDB
      BSON_Decimal128,    --  Decimal128
      BSON_MinKey,        --  Type spécial MongoDB (-inf)
      BSON_MaxKey);       --  Type spécial MongoDB (+inf)

   --  Types for binary data
   type BSON_Binary_Subtype is new Stream_Element range 0 .. 255;

   --  Forward declarations
   type BSON_Value_Type;
   type BSON_Value_Access is access BSON_Value_Type;
   type BSON_Document_Type is private;
   type BSON_Document_Access is access BSON_Document_Type;

   --  Définition pour les identifiants ObjectId
   subtype ObjectId_Type is String (1 .. 24);  --  Format hexadécimal

   --  Package for BSON arrays
   package BSON_Arrays is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => BSON_Value_Access);

   --  Package for binary data
   package Binary_Data_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Stream_Element);

   --  Types pour JavaScript et RegEx
   subtype JavaScript_Code_Type is Unbounded_String;

   --  Type pour les timestamps MongoDB
   type Timestamp_Type is record
      Increment : Unsigned_32; --  Compteur incrémental
      Seconds   : Unsigned_32; --  Secondes depuis l'epoch
   end record;

   --  Type pour les expressions régulières
   type Regex_Type is record
      Pattern : Unbounded_String; --  Motif de l'expression régulière
      Options : Unbounded_String; --  Options (i, m, x, etc.)
   end record;

   --  Type pour JavaScript avec scope
   type JS_With_Scope_Type is record
      Code  : JavaScript_Code_Type;
      Scope : BSON_Document_Access;
   end record;

   --  Type pour Decimal128
   subtype Decimal128_Type is
     Stream_Element_Array (1 .. 16); --  128 bits (16 bytes)

   type BSON_Value_Type (Kind : BSON_Type := BSON_Null) is record
      case Kind is
         when BSON_Double =>
            Double_Value : Long_Float;

         when BSON_String =>
            String_Value : Unbounded_String;

         when BSON_Document =>
            Document_Value : BSON_Document_Access;

         when BSON_Array =>
            Array_Value : BSON_Arrays.Vector;

         when BSON_Int32 =>
            Int32_Value : Integer;

         when BSON_Int64 =>
            Int64_Value : Long_Long_Integer;

         when BSON_Boolean =>
            Boolean_Value : Boolean;

         when BSON_Binary =>
            Binary_Subtype : BSON_Binary_Subtype;
            Binary_Data    : Binary_Data_Vectors.Vector;

         when BSON_ObjectId =>
            ObjectId_Value : ObjectId_Type;

         when BSON_Date =>
            Date_Value : Long_Long_Integer;

         when BSON_Regex =>
            Regex_Value : Regex_Type;

         when BSON_JavaScript =>
            JavaScript_Value : JavaScript_Code_Type;

         when BSON_JavaScript_W_Scope =>
            JS_Scope_Value : JS_With_Scope_Type;

         when BSON_Symbol =>
            Symbol_Value : Unbounded_String;

         when BSON_Timestamp =>
            Timestamp_Value : Timestamp_Type;

         when BSON_Decimal128 =>
            Decimal128_Value : Decimal128_Type;

         when BSON_DBPointer =>
            Collection : Unbounded_String;
            Pointer_Id : ObjectId_Type;

         when others =>
            null; --  BSON_Null, BSON_Undefined, BSON_MinKey, BSON_MaxKey
      end case;
   end record;

   --  Public API
   procedure Init_Document (Doc : in out BSON_Document_Type);

   procedure Add_String (Doc : in out BSON_Document_Type; Key, Value : String);

   procedure Add_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Integer);

   procedure Add_Long_Integer
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : Long_Long_Integer);

   procedure Add_Double
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Float);

   procedure Add_Boolean
     (Doc : in out BSON_Document_Type; Key : String; Value : Boolean);

   procedure Add_Document
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : BSON_Document_Type);

   procedure Add_Array (Doc : in out BSON_Document_Type; Key : String);

   procedure Add_Binary
     (Doc           : in out BSON_Document_Type;
      Key           : String;
      Subtype_Value : BSON_Binary_Subtype;
      Data          : Stream_Element_Array);

   procedure Add_ObjectId
     (Doc : in out BSON_Document_Type; Key : String; Value : ObjectId_Type);

   procedure Add_Date
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Value : Long_Long_Integer);

   --  Nouvelles procédures pour les types BSON manquants

   procedure Add_Null (Doc : in out BSON_Document_Type; Key : String);

   procedure Add_Undefined (Doc : in out BSON_Document_Type; Key : String);

   procedure Add_Regex
     (Doc     : in out BSON_Document_Type;
      Key     : String;
      Pattern : String;
      Options : String := "");

   procedure Add_JavaScript
     (Doc : in out BSON_Document_Type; Key : String; Code : String);

   procedure Add_Symbol
     (Doc : in out BSON_Document_Type; Key : String; Symbol : String);

   procedure Add_JavaScript_W_Scope
     (Doc   : in out BSON_Document_Type;
      Key   : String;
      Code  : String;
      Scope : BSON_Document_Type);

   procedure Add_Timestamp
     (Doc       : in out BSON_Document_Type;
      Key       : String;
      Seconds   : Unsigned_32;
      Increment : Unsigned_32);

   procedure Add_Decimal128
     (Doc : in out BSON_Document_Type; Key : String; Value : Decimal128_Type);

   procedure Add_MinKey (Doc : in out BSON_Document_Type; Key : String);

   procedure Add_MaxKey (Doc : in out BSON_Document_Type; Key : String);

   procedure Add_DBPointer
     (Doc        : in out BSON_Document_Type;
      Key        : String;
      Collection : String;
      ObjectId   : ObjectId_Type);

   procedure Array_Add_Value
     (Doc : in out BSON_Document_Type; Key : String; Value : BSON_Value_Type)
   with
     Pre => Key'Length > 0;

   function To_JSON (Doc : BSON_Document_Type) return String;

   function Is_Valid (Doc : BSON_Document_Type) return Boolean;

   procedure Validate (Doc : BSON_Document_Type);

   procedure To_Binary
     (Doc    : in out BSON_Document_Type;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   with
     Pre  => Buffer'Length >= 5,  --  Minimum BSON document size
     Post => Last <= Buffer'Last;

   function From_Binary
     (Buffer : Stream_Element_Array) return BSON_Document_Type
   with
     Pre => Buffer'Length >= 5;  --  Minimum BSON document size

   procedure Free (Doc : in out BSON_Document_Type);

private
   package Value_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => BSON_Value_Access);

   type BSON_Document_Type is record
      Elements : Value_Maps.Map;
      Is_Array : Boolean := False;
   end record;

   procedure Free_Document (Doc : in out BSON_Document_Type);
   procedure Free_Value (Value : in out BSON_Value_Access);
end BSON;

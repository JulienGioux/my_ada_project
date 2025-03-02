-- src/bson.ads
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Streams;            -- Ajout pour résoudre le problème de Stream_Element_Array

package BSON with SPARK_Mode is
   -- Exceptions
   Invalid_BSON_Format    : exception;
   Invalid_BSON_Type      : exception;
   BSON_Validation_Error  : exception;

   -- Types
   type BSON_Type is (
      BSON_Double,
      BSON_String,
      BSON_Document,
      BSON_Array,
      BSON_Binary,
      BSON_ObjectId,
      BSON_Boolean,
      BSON_Date,
      BSON_Null,
      BSON_Int32,
      BSON_Int64
   );

   -- Forward declarations
   type BSON_Value_Type;
   type BSON_Value_Access is access BSON_Value_Type;
   type BSON_Document_Type is private;
   type BSON_Document_Access is access BSON_Document_Type;

   -- Package for BSON arrays
   package BSON_Arrays is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => BSON_Value_Access);

   -- Binary data type pour BSON_Binary
   subtype BSON_Binary_Subtype is Natural range 0 .. 255;

   -- Binary data vector
   package Binary_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Ada.Streams.Stream_Element);

   -- ObjectId est un tableau de 12 octets
   subtype ObjectId_Type is String (1 .. 24);  -- Format hexadécimal

   type BSON_Value_Type (Kind : BSON_Type := BSON_Null) is record
      case Kind is
         when BSON_Double    => Double_Value   : Long_Float;
         when BSON_String    => String_Value   : Unbounded_String;
         when BSON_Document  => Document_Value : BSON_Document_Access;
         when BSON_Array     => Array_Value    : BSON_Arrays.Vector;
         when BSON_Int32     => Int32_Value    : Integer;
         when BSON_Int64     => Int64_Value    : Long_Long_Integer;
         when BSON_Boolean   => Boolean_Value  : Boolean;
         when BSON_Binary    =>
            Binary_Subtype   : BSON_Binary_Subtype;
            Binary_Data      : Binary_Data_Vectors.Vector;
         when BSON_ObjectId  => ObjectId_Value : ObjectId_Type;
         when BSON_Date      => Date_Value     : Long_Long_Integer; -- Millisecondes depuis l'époque Unix
         when others         => null;
      end case;
   end record;

   -- Public API
   procedure Init_Document (Doc : in out BSON_Document_Type) with
     Global => null,
     Depends => (Doc => null);

   procedure Add_String
     (Doc : in out BSON_Document_Type; Key, Value : String) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Integer) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Long_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Long_Integer) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Double
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Float) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Boolean
     (Doc : in out BSON_Document_Type; Key : String; Value : Boolean) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Document
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : BSON_Document_Type) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Array
     (Doc : in out BSON_Document_Type; Key : String) with
     Global => null,
     Depends => (Doc => (Doc, Key));

   procedure Add_Binary
     (Doc : in out BSON_Document_Type;
      Key : String;
      Subtype_Value : BSON_Binary_Subtype;
      Data : Ada.Streams.Stream_Element_Array) with
     Global => null,
     Depends => (Doc => (Doc, Key, Subtype_Value, Data));

   procedure Add_ObjectId
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : ObjectId_Type) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Date
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : Long_Long_Integer) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   procedure Array_Add_Value
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : BSON_Value_Type) with
     Global => null,
     Depends => (Doc => (Doc, Key, Value));

   function To_JSON (Doc : BSON_Document_Type) return String with
     Global => null,
     Depends => (To_JSON'Result => Doc);

   function Is_Valid (Doc : BSON_Document_Type) return Boolean with
     Global => null,
     Depends => (Is_Valid'Result => Doc);

   procedure Validate (Doc : BSON_Document_Type) with
     Global => null,
     Depends => (null => Doc);

   procedure To_Binary
     (Doc : in out BSON_Document_Type;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) with
     Global => null,
     Depends => ((Buffer, Last) => Doc);

   function From_Binary (Buffer : Ada.Streams.Stream_Element_Array)
     return BSON_Document_Type with
     Global => null,
     Depends => (From_Binary'Result => Buffer);

   -- Gestion de la mémoire
   procedure Free (Doc : in out BSON_Document_Type) with
     Global => null,
     Depends => (Doc => null);

private
   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => BSON_Value_Access);

   type BSON_Document_Type is record
      Elements : Value_Maps.Map;
      Is_Array : Boolean := False;
   end record;

   procedure Free_Document (Doc : in out BSON_Document_Type) with
     Global => null,
     Depends => (Doc => null);

   procedure Free_Value (Value : in out BSON_Value_Access) with
     Global => null,
     Depends => (Value => null);
end BSON;
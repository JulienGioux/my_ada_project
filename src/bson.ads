--    src/bson.ads
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Streams;            use Ada.Streams;

package BSON with SPARK_Mode => Off is
   --  Exceptions
   Invalid_BSON_Format    : exception;
   Invalid_BSON_Type      : exception;
   BSON_Validation_Error  : exception;

   --  Types
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
   package BSON_Arrays is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => BSON_Value_Access);

   --  Package for binary data
   package Binary_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Stream_Element);

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
         when BSON_Date      => Date_Value     : Long_Long_Integer;
         when others         => null;
      end case;
   end record;

   --  Public API
   procedure Init_Document (Doc : in out BSON_Document_Type);

   procedure Add_String
     (Doc : in out BSON_Document_Type; Key, Value : String);

   procedure Add_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Integer);

   procedure Add_Long_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Long_Integer);

   procedure Add_Double
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Float);

   procedure Add_Boolean
     (Doc : in out BSON_Document_Type; Key : String; Value : Boolean);

   procedure Add_Document
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : BSON_Document_Type);

   procedure Add_Array
     (Doc : in out BSON_Document_Type; Key : String);

   procedure Add_Binary
     (Doc : in out BSON_Document_Type;
      Key : String;
      Subtype_Value : BSON_Binary_Subtype;
      Data : Stream_Element_Array);

   procedure Add_ObjectId
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : ObjectId_Type);

   procedure Add_Date
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : Long_Long_Integer);

   procedure Array_Add_Value
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : BSON_Value_Type);

   function To_JSON (Doc : BSON_Document_Type) return String;

   function Is_Valid (Doc : BSON_Document_Type) return Boolean;

   procedure Validate (Doc : BSON_Document_Type);

   procedure To_Binary
     (Doc : in out BSON_Document_Type;
      Buffer : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   function From_Binary (Buffer : Stream_Element_Array)
     return BSON_Document_Type;

   procedure Free (Doc : in out BSON_Document_Type);

private
   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => BSON_Value_Access);

   type BSON_Document_Type is record
      Elements : Value_Maps.Map;
      Is_Array : Boolean := False;
   end record;

   procedure Free_Document (Doc : in out BSON_Document_Type);
   procedure Free_Value (Value : in out BSON_Value_Access);
end BSON;

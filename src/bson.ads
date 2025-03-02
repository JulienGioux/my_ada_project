-- src/bson.ads
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

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

   type BSON_Value_Type (Kind : BSON_Type := BSON_Null) is record
      case Kind is
         when BSON_Double    => Double_Value   : Long_Float;
         when BSON_String    => String_Value   : Unbounded_String;
         when BSON_Document  => Document_Value : BSON_Document_Access;
         when BSON_Array     => Array_Value    : BSON_Arrays.Vector;
         when BSON_Int32     => Int32_Value    : Integer;
         when BSON_Int64     => Int64_Value    : Long_Long_Integer;
         when BSON_Boolean   => Boolean_Value  : Boolean;
         when others         => null;
      end case;
   end record;

   -- Public API
   procedure Init_Document (Doc : in out BSON_Document_Type) with
     Global => null,
     Depends => (Doc => null);

   procedure Add_String
     (Doc : in out BSON_Document_Type; Key, Value : String) with
     Global => (Doc, Key, Value),
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Integer
     (Doc : in out BSON_Document_Type; Key : String; Value : Integer) with
     Global => (Doc, Key, Value),
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Double
     (Doc : in out BSON_Document_Type; Key : String; Value : Long_Float) with
     Global => (Doc, Key, Value),
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Boolean
     (Doc : in out BSON_Document_Type; Key : String; Value : Boolean) with
     Global => (Doc, Key, Value),
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Document
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : BSON_Document_Type) with
     Global => (Doc, Key, Value),
     Depends => (Doc => (Doc, Key, Value));

   procedure Add_Array
     (Doc : in out BSON_Document_Type; Key : String) with
     Global => (Doc, Key),
     Depends => (Doc => (Doc, Key));

   procedure Array_Add_Value
     (Doc : in out BSON_Document_Type;
      Key : String;
      Value : BSON_Value_Type) with
     Global => (Doc, Key, Value),
     Depends => (Doc => (Doc, Key, Value));

   function To_JSON (Doc : BSON_Document_Type) return String with
     Global => (Doc),
     Depends => (To_JSON'Result => Doc);

   function Is_Valid (Doc : BSON_Document_Type) return Boolean with
     Global => (Doc),
     Depends => (Is_Valid'Result => Doc);

   procedure Validate (Doc : BSON_Document_Type) with
     Global => (Doc),
     Depends => (Doc => Doc);

   procedure To_Binary
     (Doc : in out BSON_Document_Type;
      Buffer : out Stream_Element_Array;
      Last : out Stream_Element_Offset) with
     Global => (Doc, Buffer, Last),
     Depends => (Doc => (Doc, Buffer, Last));

   function From_Binary (Buffer : Stream_Element_Array)
     return BSON_Document_Type with
     Global => (Buffer),
     Depends => (From_Binary'Result => Buffer);

private
   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => BSON_Value_Access);

   type BSON_Document_Type is record
      Elements : Value_Maps.Map;
      Is_Array : Boolean := False;
   end record;

   procedure Free_Document (Doc : in out BSON_Document_Type) with
     Global => (Doc),
     Depends => (Doc => null);

   procedure Free_Value (Value : in out BSON_Value_Access) with
     Global => (Value),
     Depends => (Value => null);
end BSON;
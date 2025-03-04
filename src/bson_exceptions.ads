package BSON_Exceptions is
   BSON_Buffer_Overflow  : exception;
   BSON_Invalid_Encoding : exception;
   BSON_Unexpected_EOF   : exception;
   BSON_Type_Mismatch    : exception;
   BSON_Key_NotFound     : exception;
   BSON_Invalid_Format   : exception;
   BSON_Validation_Error : exception;
   Invalid_BSON_Type     : exception;
   Invalid_BSON_Format   : exception;
   BSON_Memory_Error     : exception;
end BSON_Exceptions;
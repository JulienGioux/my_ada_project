with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;        use Ada.Exceptions;
with Interfaces;            use Interfaces;
with BSON;                  use BSON;
with BSON_Exceptions;       use BSON_Exceptions;

procedure BSON_Tests is

   -- Framework de test simple
   Test_Count    : Natural := 0;
   Success_Count : Natural := 0;
   Fail_Count    : Natural := 0;

   procedure Start_Test (Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      Put ("Test " & Test_Count'Image & ": " & Test_Name & " ... ");
   end Start_Test;

   procedure Pass is
   begin
      Success_Count := Success_Count + 1;
      Put_Line ("PASS");
   end Pass;

   procedure Fail (Message : String := "") is
   begin
      Fail_Count := Fail_Count + 1;
      Put_Line ("FAIL");
      if Message'Length > 0 then
         Put_Line ("    " & Message);
      end if;
   end Fail;

   procedure Assert (Condition : Boolean; Message : String := "") is
   begin
      if Condition then
         Pass;
      else
         Fail (Message);
      end if;
   end Assert;

   procedure Summary is
   begin
      New_Line;
      Put_Line ("Test Summary:");
      Put_Line ("------------");
      Put_Line ("Total Tests:  " & Test_Count'Image);
      Put_Line ("Passed:      " & Success_Count'Image);
      Put_Line ("Failed:      " & Fail_Count'Image);
   end Summary;

   -- Fonction utilitaire pour afficher un tableau d'octets en hexadécimal
   function Hex_Dump (Data : Stream_Element_Array) return String is
      Result : Unbounded_String;
   begin
      for I in Data'Range loop
         declare
            Byte_Value : Stream_Element := Data (I);
            Hex_Value  : String (1 .. 2);
            Nibble     : Stream_Element;
         begin
            -- Traiter le premier nibble (4 bits)
            Nibble := Byte_Value / 16;
            if Nibble < 10 then
               Hex_Value (1) :=
                 Character'Val (Character'Pos ('0') + Natural (Nibble));
            else
               Hex_Value (1) :=
                 Character'Val (Character'Pos ('A') + Natural (Nibble) - 10);
            end if;

            -- Traiter le deuxième nibble (4 bits)
            Nibble := Byte_Value mod 16;
            if Nibble < 10 then
               Hex_Value (2) :=
                 Character'Val (Character'Pos ('0') + Natural (Nibble));
            else
               Hex_Value (2) :=
                 Character'Val (Character'Pos ('A') + Natural (Nibble) - 10);
            end if;

            Append (Result, Hex_Value);

            if I /= Data'Last then
               Append (Result, " ");
            end if;
         end;
      end loop;

      return To_String (Result);
   end Hex_Dump;

   -- Test de la procédure Init_Document
   procedure Test_Init_Document is
      Doc : BSON_Document_Type;
   begin
      Start_Test ("Init_Document");

      -- Test d'initialisation d'un document
      Init_Document (Doc);
      Assert (Is_Valid (Doc), "Le document initialisé devrait être valide");

      -- Vérification indirecte via To_JSON
      Assert
        (To_JSON (Doc) = "{}", "Le document initialisé devrait être vide");

      -- Libérer la mémoire
      Free (Doc);
   end Test_Init_Document;

   -- Test des procédures Add_* pour les types simples
   procedure Test_Add_Simple_Types is
      Doc : BSON_Document_Type;
   begin
      Start_Test ("Add_String");
      Init_Document (Doc);
      Add_String (Doc, "test_key", "test_value");
      Assert
        (To_JSON (Doc) = "{""test_key"": ""test_value""}",
         "La représentation JSON est incorrecte pour Add_String");
      Free (Doc);

      Start_Test ("Add_Integer");
      Init_Document (Doc);
      Add_Integer (Doc, "int_key", 42);
      Assert
        (To_JSON (Doc) = "{""int_key"": 42}",
         "La représentation JSON est incorrecte pour Add_Integer");
      Free (Doc);

      Start_Test ("Add_Long_Integer");
      Init_Document (Doc);
      Add_Long_Integer (Doc, "long_key", 1234567890123);
      Assert
        (To_JSON (Doc) = "{""long_key"": 1234567890123}",
         "La représentation JSON est incorrecte pour Add_Long_Integer");
      Free (Doc);

      Start_Test ("Add_Double");
      Init_Document (Doc);
      Add_Double (Doc, "double_key", 3.14159);
      -- Ne pas comparer directement car la précision peut varier
      declare
         JSON_Result : constant String := To_JSON (Doc);
      begin
         Assert
           (JSON_Result'Length > 15
            and then JSON_Result (1 .. 15) = "{""double_key"": ",
            "La représentation JSON est incorrecte pour Add_Double");
      end;
      Free (Doc);

      Start_Test ("Add_Boolean (True)");
      Init_Document (Doc);
      Add_Boolean (Doc, "bool_key", True);
      Assert
        (To_JSON (Doc) = "{""bool_key"": true}",
         "La représentation JSON est incorrecte pour Add_Boolean(True)");
      Free (Doc);

      Start_Test ("Add_Boolean (False)");
      Init_Document (Doc);
      Add_Boolean (Doc, "bool_key", False);
      Assert
        (To_JSON (Doc) = "{""bool_key"": false}",
         "La représentation JSON est incorrecte pour Add_Boolean(False)");
      Free (Doc);

      Start_Test ("Add_Null");
      Init_Document (Doc);
      Add_Null (Doc, "null_key");
      Assert
        (To_JSON (Doc) = "{""null_key"": null}",
         "La représentation JSON est incorrecte pour Add_Null");
      Free (Doc);
   end Test_Add_Simple_Types;

   -- Test pour les documents imbriqués
   procedure Test_Nested_Documents is
      Main_Doc : BSON_Document_Type;
      Sub_Doc  : BSON_Document_Type;
   begin
      Start_Test ("Add_Document (document imbriqué)");

      -- Initialiser les documents
      Init_Document (Main_Doc);
      Init_Document (Sub_Doc);

      -- Ajouter des données au sous-document
      Add_String (Sub_Doc, "city", "Paris");
      Add_String (Sub_Doc, "country", "France");

      -- Ajouter le sous-document au document principal
      Add_Document (Main_Doc, "address", Sub_Doc);

      -- Vérifier la représentation JSON
      Assert
        (To_JSON (Main_Doc)
         = "{""address"": {""city"": ""Paris"", ""country"": ""France""}}",
         "La représentation JSON est incorrecte pour les documents imbriqués");

      -- Libérer la mémoire
      Free (Main_Doc);
   end Test_Nested_Documents;

   -- Test pour les tableaux
   procedure Test_Arrays is
      Doc : BSON_Document_Type;
   begin
      Start_Test ("Add_Array et Array_Add_Value");

      -- Initialiser le document
      Init_Document (Doc);

      -- Créer un tableau
      Add_Array (Doc, "colors");

      -- Ajouter des valeurs au tableau
      Array_Add_Value
        (Doc,
         "colors",
         (Kind => BSON_String, String_Value => To_Unbounded_String ("red")));
      Array_Add_Value
        (Doc,
         "colors",
         (Kind => BSON_String, String_Value => To_Unbounded_String ("green")));
      Array_Add_Value
        (Doc,
         "colors",
         (Kind => BSON_String, String_Value => To_Unbounded_String ("blue")));

      -- Vérifier la représentation JSON
      Assert
        (To_JSON (Doc) = "{""colors"": [""red"", ""green"", ""blue""]}",
         "La représentation JSON est incorrecte pour les tableaux");

      -- Libérer la mémoire
      Free (Doc);
   exception
      when E : others =>
         Fail ("Exception inattendue: " & Exception_Name (E));
         Free (Doc);
   end Test_Arrays;

   -- Test pour Binary
   procedure Test_Binary_Data is
      Doc  : BSON_Document_Type;
      Data : constant Stream_Element_Array (1 .. 4) :=
        (16#48#, 16#65#, 16#6C#, 16#6C#);  -- "Hell" en ASCII
   begin
      Start_Test ("Add_Binary");

      -- Initialiser le document
      Init_Document (Doc);

      -- Ajouter des données binaires
      Add_Binary (Doc, "binary_data", 0, Data);

      -- Vérifier que le document est valide
      Assert
        (Is_Valid (Doc), "Document avec données binaires devrait être valide");

      -- Libérer la mémoire
      Free (Doc);
   end Test_Binary_Data;

   -- Test pour To_Binary et From_Binary
   procedure Test_Binary_Conversion is
      Doc, Reconstructed_Doc : BSON_Document_Type;
      Buffer                 :
        Stream_Element_Array (1 .. 1000);  -- Taille suffisante pour nos tests
      Last                   : Stream_Element_Offset;
   begin
      Start_Test ("To_Binary et From_Binary (document simple)");

      -- Initialiser et remplir un document
      Init_Document (Doc);
      Add_String (Doc, "name", "John");
      Add_Integer (Doc, "age", 30);

      -- Convertir en binaire
      To_Binary (Doc, Buffer, Last);

      -- Afficher les premiers octets pour debugging
      Put_Line ("BSON Hex: " & Hex_Dump (Buffer (1 .. Last)));

      -- Reconstruire à partir du binaire
      Reconstructed_Doc := From_Binary (Buffer (1 .. Last));

      -- Vérifier que le document reconstruit est identique à l'original
      Assert
        (To_JSON (Reconstructed_Doc) = To_JSON (Doc),
         "Le document reconstruit devrait être identique à l'original");

      -- Libérer la mémoire
      Free (Doc);
      Free (Reconstructed_Doc);
   end Test_Binary_Conversion;

   -- Test pour To_Binary et From_Binary avec tous types BSON
   procedure Test_All_BSON_Types is
      Doc, Reconstructed_Doc : BSON_Document_Type;
      Sub_Doc                : BSON_Document_Type;
      Buffer                 :
        Stream_Element_Array (1 .. 2000);  -- Taille suffisante pour nos tests
      Last                   : Stream_Element_Offset;
      Binary_Data            : constant Stream_Element_Array (1 .. 3) :=
        (1, 2, 3);
      Dec128                 : constant Decimal128_Type := (others => 0);
      Object_Id              : constant ObjectId_Type :=
        "123456789012345678901234";
   begin
      Start_Test ("Conversion binaire avec tous les types BSON");

      -- Initialiser documents
      Init_Document (Doc);
      Init_Document (Sub_Doc);

      -- Ajouter des exemples de tous types BSON
      Add_String (Doc, "string_key", "string_value");
      Add_Integer (Doc, "int32_key", 42);
      Add_Long_Integer (Doc, "int64_key", 1234567890123);
      Add_Double (Doc, "double_key", 3.14159);
      Add_Boolean (Doc, "bool_key", True);

      -- Sous-document
      Add_String (Sub_Doc, "sub_key", "sub_value");
      Add_Document (Doc, "doc_key", Sub_Doc);

      -- Tableau
      Add_Array (Doc, "array_key");
      Array_Add_Value
        (Doc,
         "array_key",
         (Kind => BSON_String, String_Value => To_Unbounded_String ("item1")));
      Array_Add_Value
        (Doc,
         "array_key",
         (Kind => BSON_String, String_Value => To_Unbounded_String ("item2")));

      -- Types spéciaux
      Add_Binary (Doc, "binary_key", 0, Binary_Data);
      Add_ObjectId (Doc, "objectid_key", Object_Id);
      Add_Date
        (Doc,
         "date_key",
         1609459200000);  -- 2021-01-01 en millisecondes depuis epoch
      Add_Null (Doc, "null_key");
      Add_Undefined (Doc, "undefined_key");
      Add_Regex (Doc, "regex_key", "^test$", "i");
      Add_JavaScript (Doc, "js_key", "function() { return true; }");
      Add_Symbol (Doc, "symbol_key", "symbol_value");
      Add_JavaScript_W_Scope
        (Doc, "js_scope_key", "function() { return x; }", Sub_Doc);
      Add_Timestamp (Doc, "timestamp_key", 1634000000, 1);
      Add_Decimal128 (Doc, "decimal_key", Dec128);
      Add_MinKey (Doc, "minkey");
      Add_MaxKey (Doc, "maxkey");

      -- Convertir en binaire
      To_Binary (Doc, Buffer, Last);

      -- Reconstruire à partir du binaire
      Reconstructed_Doc := From_Binary (Buffer (1 .. Last));

      -- Vérifier que le document reconstruit est valide
      Assert
        (Is_Valid (Reconstructed_Doc),
         "Le document reconstruit devrait être valide");

      -- Libérer la mémoire
      Free (Doc);
      Free (Reconstructed_Doc);
   end Test_All_BSON_Types;

   -- Test pour les erreurs et cas limites
   procedure Test_Error_Handling is
      Doc              : BSON_Document_Type;
      Small_Buffer     :
        Stream_Element_Array
          (1 .. 5);  -- Trop petit pour contenir un document non-trivial
      Last             : Stream_Element_Offset;
      Invalid_Buffer   : Stream_Element_Array (1 .. 10) :=
        (others => 42);  -- Données invalides
      Exception_Raised : Boolean;
   begin
      -- Test avec un buffer trop petit
      Start_Test ("Buffer_Overflow Detection");

      Init_Document (Doc);
      Add_String
        (Doc, "key", "value");  -- Ce document est plus grand que 5 octets

      Exception_Raised := False;
      begin
         To_Binary (Doc, Small_Buffer, Last);
      exception
         when BSON_Buffer_Overflow =>
            Exception_Raised := True;
      end;

      Assert
        (Exception_Raised,
         "BSON_Buffer_Overflow devrait être levé pour un buffer trop petit");

      -- Test avec des données BSON invalides
      Start_Test ("Invalid BSON Format Detection");

      Exception_Raised := False;
      begin
         Doc := From_Binary (Invalid_Buffer);
         -- Cette fonction ne lève pas d'exception, elle renvoie un document vide
         -- Donc on vérifie juste que le document retourné est vide
         Assert
           (To_JSON (Doc) = "{}",
            "From_Binary devrait retourner un document vide pour des données invalides");
         Pass;
      exception
         when others =>
            Fail
              ("From_Binary ne devrait pas lever d'exception pour des données invalides");
      end;

      Free (Doc);
   end Test_Error_Handling;

begin
   Put_Line ("Exécution des tests unitaires BSON");
   Put_Line ("================================");
   New_Line;

   -- Exécuter tous les tests
   Test_Init_Document;
   Test_Add_Simple_Types;
   Test_Nested_Documents;
   Test_Arrays;
   Test_Binary_Data;
   Test_Binary_Conversion;
   Test_All_BSON_Types;
   Test_Error_Handling;

   -- Afficher le résumé
   Summary;
end BSON_Tests;

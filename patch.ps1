# fix_all_warnings.ps1
$file = "src\bson.adb"
$content = Get-Content -Path $file -Raw

# 1. Ajouter des espaces après "--" pour les commentaires
$content = $content -replace '(^|\r\n|\n)--([^ ])', '$1--  $2'

# 2. Ajouter des espaces avant ":" dans les déclarations
$content = $content -replace '([a-zA-Z0-9_]\s*):(\s*[a-zA-Z])', '$1 :$2'

# 3. Marquer les variables non utilisées avec "pragma Unreferenced"
$content = $content -replace 'Array_Doc\s*:', 'Array_Doc : pragma Unreferenced;'
$content = $content -replace '\bE\s*:', 'E : pragma Unreferenced;'

# 4. Rendre constantes les variables qui ne sont pas modifiées
$content = $content -replace 'Array_Value\s*:', 'Array_Value : constant'
$content = $content -replace 'Element\s*:', 'Element : constant'

# 5. Supprimer les constantes non utilisées en les commentant
$constPattern = '(BSON_TYPE_[A-Z_]+\s*:\s*constant\s*:=\s*[^;]+;)'
$unusedConsts = @(
    "BSON_TYPE_UNDEFINED",
    "BSON_TYPE_REGEX",
    "BSON_TYPE_DBPOINTER",
    "BSON_TYPE_JAVASCRIPT",
    "BSON_TYPE_SYMBOL",
    "BSON_TYPE_JAVASCRIPT_W_SCOPE",
    "BSON_TYPE_TIMESTAMP",
    "BSON_TYPE_DECIMAL128",
    "BSON_TYPE_MINKEY",
    "BSON_TYPE_MAXKEY"
)

foreach ($const in $unusedConsts) {
    $content = $content -replace "($const\s*:\s*constant\s*:=\s*[^;]+;)", "--  $1 --  Non utilisé"
}

# 6. Marquer les fonctions non utilisées
$content = $content -replace '(function Hex_To_Stream_Element.*?return Stream_Element is)', '$1 pragma Unreferenced;'

# 7. Ajouter un pragma pour ignorer les avertissements "others" redondants
$content = $content -replace '(when others =>)', 'pragma Warnings (Off); $1 pragma Warnings (On);'

# 8. Supprimer les lignes vides à la fin du fichier
$content = $content -replace '(\r?\n)+\s*$', '$1'

# Sauvegarder les modifications
Set-Content -Path $file -Value $content

Write-Host "Corrections des avertissements appliquées au fichier $file" -ForegroundColor Green

# Maintenant, corriger les lignes trop longues
$contentLines = $content -split '(\r?\n)'
$outputLines = @()
$pattern = '.{1,75}(\s|$)'

foreach ($line in $contentLines) {
    if ($line.Length -gt 79) {
        # Séparer les lignes d'expression trop longues
        if ($line -match '(\s*)(.*?)\s*:=\s*(.*?)\s*;') {
            $indent = $matches[1]
            $variable = $matches[2]
            $expression = $matches[3]
            
            if ($expression.Length -gt 60) {
                $outputLines += "$indent$variable :="
                $outputLines += "$indent  $expression;"
            }
            else {
                $outputLines += $line
            }
        }
        # Séparer les lignes d'appel de fonction trop longues
        elseif ($line -match '(\s*)(.*?\()(.*?)(\).*)') {
            $indent = $matches[1]
            $funcStart = $matches[2]
            $params = $matches[3]
            $funcEnd = $matches[4]
            
            if ($params.Length -gt 50) {
                $paramList = $params -split ','
                $outputLines += "$indent$funcStart"
                foreach ($param in $paramList) {
                    $outputLines += "$indent  $param.Trim(),"
                }
                # Supprimer la dernière virgule et ajouter la fin
                $outputLines[-1] = $outputLines[-1].TrimEnd(',')
                $outputLines += "$indent$funcEnd"
            }
            else {
                $outputLines += $line
            }
        }
        else {
            $outputLines += $line
        }
    }
    else {
        $outputLines += $line
    }
}

$newContent = $outputLines -join ""
Set-Content -Path $file -Value $newContent
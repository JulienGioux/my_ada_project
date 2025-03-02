# fix_comments_improved.ps1
# Script pour corriger le format des commentaires dans les fichiers Ada
# Les commentaires doivent avoir EXACTEMENT deux espaces après "--"

Write-Host "Correction du format des commentaires dans les fichiers Ada..." -ForegroundColor Cyan
$count = 0
$filesModified = 0

# Répertoire source
$srcDir = Join-Path -Path $PSScriptRoot -ChildPath "src"
if (-not (Test-Path $srcDir)) {
    Write-Host "Répertoire source introuvable: $srcDir" -ForegroundColor Red
    exit 1
}

# Récupérer tous les fichiers Ada (.ads et .adb)
$adaFiles = Get-ChildItem -Path $srcDir -Include "*.adb", "*.ads" -Recurse

foreach ($file in $adaFiles) {
    $fileName = $file.Name
    Write-Host "Traitement de $fileName..." -NoNewline
    $modified = $false
    
    # Lire toutes les lignes du fichier
    $lines = [System.IO.File]::ReadAllLines($file.FullName)
    $newLines = @()
    
    foreach ($line in $lines) {
        # Cherche les lignes contenant des commentaires
        if ($line -match "--") {
            # Diviser la ligne au premier commentaire
            $parts = $line -split "--", 2
            
            if ($parts.Count -eq 2) {
                $beforeComment = $parts[0]
                $afterComment = $parts[1]
                
                # Si le commentaire commence par un espace, ajouter un espace supplémentaire
                if ($afterComment.StartsWith(" ") -and -not $afterComment.StartsWith("  ")) {
                    $afterComment = " " + $afterComment
                    $newLine = $beforeComment + "--" + $afterComment
                    $newLines += $newLine
                    $modified = $true
                    continue
                }
                # Si le commentaire ne commence pas par un espace, ajouter deux espaces
                elseif (-not $afterComment.StartsWith(" ")) {
                    $afterComment = "  " + $afterComment
                    $newLine = $beforeComment + "--" + $afterComment
                    $newLines += $newLine
                    $modified = $true
                    continue
                }
            }
        }
        
        # Si on n'a pas modifié la ligne, l'ajouter telle quelle
        $newLines += $line
    }
    
    # Si des modifications ont été apportées, écrire le fichier
    if ($modified) {
        [System.IO.File]::WriteAllLines($file.FullName, $newLines)
        Write-Host " Modifié!" -ForegroundColor Green
        $filesModified++
    }
    else {
        Write-Host " Aucune modification nécessaire" -ForegroundColor Gray
    }
    
    $count++
}

Write-Host ""
Write-Host "Traitement terminé!" -ForegroundColor Green
Write-Host "$filesModified fichiers ont été modifiés sur $count." -ForegroundColor Cyan
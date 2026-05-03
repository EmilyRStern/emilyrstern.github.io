# =============================================================================
# cleanup.ps1 — one-time repo cleanup
# =============================================================================
# Run from the repo root:
#     cd C:\Users\erose\Documents\GitHub\emilyrstern.github.io
#     powershell -ExecutionPolicy Bypass -File .\cleanup.ps1
#
# What it does:
#   1. Renames "(1)"-suffixed PDFs back to canonical names
#   2. Deletes duplicate / orphan files agreed during the cleanup pass
#   3. Removes the multi-GB data tree from git tracking (files stay on disk)
#   4. Prints a summary
#
# Safe to re-run; every step skips if the target is already gone.
# Delete this script after you've run it once and committed the results.
# =============================================================================

$ErrorActionPreference = "Continue"
$repoRoot = $PSScriptRoot
Set-Location $repoRoot

Write-Host ""
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host " emilyrstern.github.io — repo cleanup" -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host "Working directory: $repoRoot" -ForegroundColor DarkGray
Write-Host ""

$deleted = @()
$renamed = @()
$skipped = @()

function Remove-IfExists($path, $description) {
    $full = Join-Path $repoRoot $path
    if (Test-Path -LiteralPath $full) {
        try {
            Remove-Item -LiteralPath $full -Recurse -Force -ErrorAction Stop
            $script:deleted += "$path  ($description)"
            Write-Host "  [deleted] $path" -ForegroundColor Green
        } catch {
            Write-Host "  [FAILED ] $path  -- $($_.Exception.Message)" -ForegroundColor Red
        }
    } else {
        $script:skipped += "$path  (already gone)"
    }
}

function Rename-IfExists($oldName, $newName, $description) {
    $oldFull = Join-Path $repoRoot $oldName
    $newFull = Join-Path $repoRoot $newName
    if (Test-Path -LiteralPath $oldFull) {
        if (Test-Path -LiteralPath $newFull) {
            Write-Host "  [skip   ] $oldName  -> target $newName already exists" -ForegroundColor Yellow
            $script:skipped += "$oldName  (target exists)"
        } else {
            try {
                Move-Item -LiteralPath $oldFull -Destination $newFull -Force -ErrorAction Stop
                $script:renamed += "$oldName  ->  $newName  ($description)"
                Write-Host "  [renamed] $oldName  ->  $newName" -ForegroundColor Green
            } catch {
                Write-Host "  [FAILED ] $oldName  -- $($_.Exception.Message)" -ForegroundColor Red
            }
        }
    } else {
        $script:skipped += "$oldName  (already gone)"
    }
}

# ---------------------------------------------------------------------------
# 1. Rename "(1)" suffixed PDFs back to canonical names
# ---------------------------------------------------------------------------
Write-Host "Step 1: rename `"(1)`" suffixed PDFs" -ForegroundColor Cyan
Rename-IfExists `
    "knowledge_mining\assignments\KM_Assignment01 (1).pdf" `
    "knowledge_mining\assignments\KM_Assignment01.pdf" `
    "drop download-suffix"
Rename-IfExists `
    "knowledge_mining\assignments\KM_Assignment02 (1).pdf" `
    "knowledge_mining\assignments\KM_Assignment02.pdf" `
    "drop download-suffix"
Write-Host ""

# ---------------------------------------------------------------------------
# 2. Delete duplicate (1)-suffixed source files (canonical versions exist)
# ---------------------------------------------------------------------------
Write-Host "Step 2: delete duplicate (1) files" -ForegroundColor Cyan
Remove-IfExists "knowledge_mining\assignments\Lab01 (1).qmd" "duplicate of knowledge_mining\Lab01.qmd"
Remove-IfExists "knowledge_mining\assignments\Lab02 (1).qmd" "duplicate of knowledge_mining\Lab02.qmd"
Remove-IfExists "Emily Stern Research CV.PDF"      "uppercase duplicate of .pdf"
Remove-IfExists "docs\Emily Stern Research CV.PDF" "uppercase duplicate of .pdf in docs/"
Remove-IfExists "Emily Stern Headshot.jpg"         "duplicate of docs/Emily Stern Headshot.jpg"
Write-Host ""

# ---------------------------------------------------------------------------
# 3. Delete unreferenced docs/ artifacts
# ---------------------------------------------------------------------------
Write-Host "Step 3: delete unreferenced docs/ artifacts" -ForegroundColor Cyan
Remove-IfExists "docs\DCP.html"   "no source qmd"
Remove-IfExists "docs\docs"       "nested artifact folder"
Write-Host ""

# ---------------------------------------------------------------------------
# 4. Delete orphan _site/ (replaced by docs/ via output-dir)
# ---------------------------------------------------------------------------
Write-Host "Step 4: delete orphan _site/" -ForegroundColor Cyan
Remove-IfExists "_site" "old Quarto output dir, replaced by docs/"
Write-Host ""

# ---------------------------------------------------------------------------
# 5. Untrack the big data tree (stays on disk; gitignore now applies)
# ---------------------------------------------------------------------------
Write-Host "Step 5: remove large data folders from git tracking" -ForegroundColor Cyan
$dataPaths = @(
    "info_management/final_project/data"
)
foreach ($p in $dataPaths) {
    $full = Join-Path $repoRoot $p
    if (Test-Path -LiteralPath $full) {
        Write-Host "  [git rm --cached -r] $p" -ForegroundColor Green
        & git rm --cached -r --quiet -- $p 2>&1 | Out-Null
        if ($LASTEXITCODE -eq 0) {
            $script:deleted += "$p  (untracked from git, files preserved on disk)"
        } else {
            Write-Host "  [info] $p was already untracked or git not available" -ForegroundColor Yellow
        }
    } else {
        Write-Host "  [skip] $p does not exist locally" -ForegroundColor DarkGray
    }
}

# Also untrack any *.duckdb / *.parquet / *.zip / *.rds that were committed
$bigExt = @("*.duckdb", "*.duckdb.wal", "*.parquet", "*.rds", "*.RDS", "*.zip")
foreach ($pattern in $bigExt) {
    $tracked = & git ls-files -- $pattern 2>$null
    if ($tracked) {
        foreach ($f in $tracked) {
            Write-Host "  [git rm --cached]  $f" -ForegroundColor Green
            & git rm --cached --quiet -- $f 2>&1 | Out-Null
            $script:deleted += "$f  (untracked, .gitignore now matches)"
        }
    }
}
Write-Host ""

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host " Summary" -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Renamed ($($renamed.Count)):" -ForegroundColor White
$renamed | ForEach-Object { Write-Host "  $_" -ForegroundColor DarkGray }
Write-Host ""
Write-Host "Deleted / untracked ($($deleted.Count)):" -ForegroundColor White
$deleted | ForEach-Object { Write-Host "  $_" -ForegroundColor DarkGray }
Write-Host ""
Write-Host "Skipped — already gone or n/a ($($skipped.Count)):" -ForegroundColor DarkGray
if ($skipped.Count -gt 10) {
    $skipped | Select-Object -First 5 | ForEach-Object { Write-Host "  $_" -ForegroundColor DarkGray }
    Write-Host "  ... and $($skipped.Count - 5) more" -ForegroundColor DarkGray
} else {
    $skipped | ForEach-Object { Write-Host "  $_" -ForegroundColor DarkGray }
}
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "  1. Inspect changes:     git status" -ForegroundColor White
Write-Host "  2. Re-render the site:  quarto render" -ForegroundColor White
Write-Host "  3. Commit:              git add -A; git commit -m `"Cleanup: untrack data, remove orphans`"" -ForegroundColor White
Write-Host "  4. Remove this script:  Remove-Item .\cleanup.ps1" -ForegroundColor White
Write-Host ""

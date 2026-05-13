param(
  [string]$OutputZip = "release\glassjar-windows.zip"
)

$ErrorActionPreference = 'Stop'

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $root

$exe = Get-ChildItem -Path "dist-newstyle" -Recurse -File -Filter "glassjar.exe" |
  Sort-Object LastWriteTime -Descending |
  Select-Object -First 1

if (-not $exe) {
  throw "glassjar.exe not found. Run cabal build first."
}

if (-not (Test-Path "data")) {
  throw "data directory not found."
}

$staging = Join-Path $root "release\glassjar"
if (Test-Path $staging) {
  Remove-Item -Recurse -Force $staging
}
New-Item -ItemType Directory -Force -Path $staging | Out-Null

Copy-Item -Path $exe.FullName -Destination (Join-Path $staging "glassjar.exe") -Force
Copy-Item -Path "data" -Destination (Join-Path $staging "data") -Recurse -Force

$zipPath = Join-Path $root $OutputZip
$zipDir = Split-Path -Parent $zipPath
if (-not (Test-Path $zipDir)) {
  New-Item -ItemType Directory -Force -Path $zipDir | Out-Null
}
if (Test-Path $zipPath) {
  Remove-Item -Force $zipPath
}

Compress-Archive -Path (Join-Path $staging "*") -DestinationPath $zipPath -CompressionLevel Optimal
Write-Host "Packed: $zipPath"

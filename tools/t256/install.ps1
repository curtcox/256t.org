<#
.SYNOPSIS
  Install the t256 command line tool on Windows.

.DESCRIPTION
  Downloads t256.exe from the GitHub release, checks its SHA-256 checksum,
  and installs it to %LOCALAPPDATA%\Programs\t256. Unless -NoPath is given,
  that directory is added to your user PATH.

  irm https://raw.githubusercontent.com/curtcox/256t.org/main/tools/t256/install.ps1 | iex

.PARAMETER Version
  Release tag to install, e.g. t256-v0.1.0 (default: $env:T256_VERSION or latest).

.PARAMETER InstallDir
  Directory to install into (default: $env:T256_INSTALL_DIR or
  %LOCALAPPDATA%\Programs\t256).

.PARAMETER BaseUrl
  Download from this URL instead of GitHub releases (default: $env:T256_BASE_URL).

.PARAMETER NoPath
  Do not add the install directory to the user PATH.
#>
param(
  [string]$Version = $env:T256_VERSION,
  [string]$InstallDir = $env:T256_INSTALL_DIR,
  [string]$BaseUrl = $env:T256_BASE_URL,
  [switch]$NoPath
)

$ErrorActionPreference = 'Stop'
$repo = 'curtcox/256t.org'

$arch = switch ($env:PROCESSOR_ARCHITECTURE) {
  'AMD64' { 'amd64' }
  'ARM64' { 'arm64' }
  default { throw "t256 install: unsupported CPU $($env:PROCESSOR_ARCHITECTURE)" }
}
$name = "t256-windows-$arch.exe"
if ($BaseUrl) {
  $base = $BaseUrl.TrimEnd('/')
} elseif ($Version) {
  $base = "https://github.com/$repo/releases/download/$Version"
} else {
  $base = "https://github.com/$repo/releases/latest/download"
}
if (-not $InstallDir) {
  $InstallDir = Join-Path $env:LOCALAPPDATA 'Programs\t256'
}
New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null

$tmp = Join-Path ([System.IO.Path]::GetTempPath()) ("t256-" + [guid]::NewGuid())
New-Item -ItemType Directory -Path $tmp | Out-Null
try {
  Write-Host "Downloading $base/$name"
  $ProgressPreference = 'SilentlyContinue'
  Invoke-WebRequest -UseBasicParsing -Uri "$base/$name" -OutFile (Join-Path $tmp $name)
  Invoke-WebRequest -UseBasicParsing -Uri "$base/SHA256SUMS" -OutFile (Join-Path $tmp 'SHA256SUMS')

  $expected = (Get-Content (Join-Path $tmp 'SHA256SUMS') |
    Where-Object { ($_ -split '\s+')[1] -in @($name, "*$name") } |
    ForEach-Object { ($_ -split '\s+')[0] }) | Select-Object -First 1
  $actual = (Get-FileHash -Algorithm SHA256 (Join-Path $tmp $name)).Hash.ToLower()
  if (-not $expected -or $expected.ToLower() -ne $actual) {
    throw "t256 install: checksum mismatch for $name"
  }

  $target = Join-Path $InstallDir 't256.exe'
  Move-Item -Force (Join-Path $tmp $name) $target
  Write-Host "Installed $(& $target version) to $target"
} finally {
  Remove-Item -Recurse -Force $tmp -ErrorAction SilentlyContinue
}

if (-not $NoPath) {
  $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
  if (($userPath -split ';') -notcontains $InstallDir) {
    $newPath = if ($userPath) { "$userPath;$InstallDir" } else { $InstallDir }
    [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')
    Write-Host "Added $InstallDir to your user PATH. Open a new terminal to use t256."
  }
}

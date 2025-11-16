$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$repoRoot = Resolve-Path (Join-Path $scriptDir '..' '..')
$cidsDir = Join-Path $repoRoot 'cids'

function To-Base64Url {
    param(
        [byte[]] $Bytes
    )

    return [Convert]::ToBase64String($Bytes).TrimEnd('=') -replace '\+','-' -replace '/','_'
}

function Encode-Length {
    param(
        [int] $Length
    )

    $lengthBytes = New-Object byte[] 6
    for ($i = 5; $i -ge 0; $i--) {
        $shift = 8 * $i
        $lengthBytes[5 - $i] = [byte](($Length -shr $shift) -band 0xFF)
    }

    return To-Base64Url -Bytes $lengthBytes
}

function Compute-Cid {
    param(
        [byte[]] $Content
    )

    $prefix = Encode-Length -Length $Content.Length

    if ($Content.Length -le 64) {
        $suffix = To-Base64Url -Bytes $Content
    }
    else {
        $sha = [System.Security.Cryptography.SHA512]::Create()
        $hash = $sha.ComputeHash($Content)
        $suffix = To-Base64Url -Bytes $hash
    }

    return "$prefix$suffix"
}

function Main {
    $mismatches = @()
    $count = 0

    Get-ChildItem -Path $cidsDir | Where-Object { -not $_.PSIsContainer } | Sort-Object Name | ForEach-Object {
        $count++
        $content = [IO.File]::ReadAllBytes($_.FullName)
        $expected = Compute-Cid -Content $content
        if ($_.Name -ne $expected) {
            $mismatches += [PSCustomObject]@{ Actual = $_.Name; Expected = $expected }
        }
    }

    if ($mismatches.Count -gt 0) {
        Write-Output "Found CID mismatches:"
        foreach ($entry in $mismatches) {
            Write-Output "- $($entry.Actual) should be $($entry.Expected)"
        }
        return 1
    }

    Write-Output "All $count CID files match their contents."
    return 0
}

exit (Main)

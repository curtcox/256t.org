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

function Download-Cid {
    param(
        [string] $BaseUrl,
        [string] $Cid
    )

    $url = "$($BaseUrl.TrimEnd('/'))/$Cid"
    
    try {
        $webClient = New-Object System.Net.WebClient
        $content = $webClient.DownloadData($url)
        $computed = Compute-Cid -Content $content
        $isValid = $computed -eq $Cid
        
        return @{
            Content = $content
            Computed = $computed
            IsValid = $isValid
        }
    }
    catch {
        throw $_.Exception.Message
    }
}

function Main {
    $mismatches = @()
    $downloadFailures = @()
    $count = 0
    $baseUrl = "https://256t.org"

    Get-ChildItem -Path $cidsDir | Where-Object { -not $_.PSIsContainer } | Sort-Object Name | ForEach-Object {
        $count++
        $cid = $_.Name
        $localContent = [IO.File]::ReadAllBytes($_.FullName)
        $expected = Compute-Cid -Content $localContent
        
        # Check local CID file
        if ($cid -ne $expected) {
            $mismatches += [PSCustomObject]@{ Cid = $cid; Expected = $expected }
        }
        
        # Check downloaded content
        try {
            $result = Download-Cid -BaseUrl $baseUrl -Cid $cid
            if (-not $result.IsValid) {
                $downloadFailures += [PSCustomObject]@{ Cid = $cid; Error = $result.Computed }
            }
            elseif (-not (Compare-Object -ReferenceObject $result.Content -DifferenceObject $localContent -SyncWindow 0)) {
                # Arrays are equal (no differences found)
            }
            else {
                $downloadFailures += [PSCustomObject]@{ Cid = $cid; Error = "content mismatch with local file" }
            }
        }
        catch {
            $downloadFailures += [PSCustomObject]@{ Cid = $cid; Error = $_.Exception.Message }
        }
    }

    $hasErrors = $false

    if ($mismatches.Count -gt 0) {
        Write-Output "Found CID mismatches:"
        foreach ($entry in $mismatches) {
            Write-Output "- $($entry.Cid) should be $($entry.Expected)"
        }
        $hasErrors = $true
    }

    if ($downloadFailures.Count -gt 0) {
        Write-Error "Found download validation failures:"
        foreach ($entry in $downloadFailures) {
            Write-Error "- $($entry.Cid): $($entry.Error)"
        }
        $hasErrors = $true
    }

    if ($hasErrors) {
        return 1
    }

    Write-Output "All $count CID files match their contents."
    Write-Output "All $count downloaded CIDs are valid."
    return 0
}

exit (Main)

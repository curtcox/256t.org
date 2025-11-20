<?php

declare(strict_types=1);

require __DIR__ . '/cid.php';

$entries = scandir(CIDS_DIR);
if ($entries === false) {
    fwrite(STDERR, "Unable to read cids directory.\n");
    exit(1);
}

sort($entries);

$mismatches = [];
$download_failures = [];
$count = 0;
$base_url = 'https://256t.org';

foreach ($entries as $entry) {
    if ($entry === '.' || $entry === '..') {
        continue;
    }

    $path = CIDS_DIR . DIRECTORY_SEPARATOR . $entry;
    if (is_dir($path)) {
        continue;
    }

    $local_content = file_get_contents($path);
    if ($local_content === false) {
        $mismatches[] = "$entry could not be read";
        continue;
    }

    $count++;
    $expected = compute_cid($local_content);
    if ($expected !== $entry) {
        $mismatches[] = sprintf('%s should be %s', $entry, $expected);
    }
    
    // Check downloaded content
    try {
        $result = download_cid($base_url, $entry);
        if (!$result['is_valid']) {
            $download_failures[] = [$entry, $result['computed']];
        } elseif ($result['content'] !== $local_content) {
            $download_failures[] = [$entry, 'content mismatch with local file'];
        }
    } catch (Exception $e) {
        $download_failures[] = [$entry, $e->getMessage()];
    }
}

$has_errors = false;

if ($mismatches) {
    echo "Found CID mismatches:\n";
    foreach ($mismatches as $message) {
        echo "- {$message}\n";
    }
    $has_errors = true;
}

if ($download_failures) {
    fwrite(STDERR, "Found download validation failures:\n");
    foreach ($download_failures as [$cid, $error]) {
        fwrite(STDERR, "- {$cid}: {$error}\n");
    }
    $has_errors = true;
}

if ($has_errors) {
    exit(1);
}

echo "All {$count} CID files match their contents.\n";
echo "All {$count} downloaded CIDs are valid.\n";

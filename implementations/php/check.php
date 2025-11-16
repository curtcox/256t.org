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
$count = 0;

foreach ($entries as $entry) {
    if ($entry === '.' || $entry === '..') {
        continue;
    }

    $path = CIDS_DIR . DIRECTORY_SEPARATOR . $entry;
    if (is_dir($path)) {
        continue;
    }

    $content = file_get_contents($path);
    if ($content === false) {
        $mismatches[] = "$entry could not be read";
        continue;
    }

    $count++;
    $expected = compute_cid($content);
    if ($expected !== $entry) {
        $mismatches[] = sprintf('%s should be %s', $entry, $expected);
    }
}

if ($mismatches) {
    echo "Found CID mismatches:\n";
    foreach ($mismatches as $message) {
        echo "- {$message}\n";
    }
    exit(1);
}

echo "All {$count} CID files match their contents.\n";

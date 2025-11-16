<?php

declare(strict_types=1);

$baseDir = realpath(__DIR__ . '/../../');

define('BASE_DIR', $baseDir === false ? '' : $baseDir);
define('EXAMPLES_DIR', BASE_DIR . '/examples');
define('CIDS_DIR', BASE_DIR . '/cids');

function base64url_encode(string $data): string
{
    return rtrim(strtr(base64_encode($data), '+/', '-_'), '=');
}

function encode_length(int $length): string
{
    $bytes = '';
    for ($i = 5; $i >= 0; $i--) {
        $bytes .= chr(($length >> ($i * 8)) & 0xFF);
    }

    return base64url_encode($bytes);
}

function compute_cid(string $content): string
{
    $prefix = encode_length(strlen($content));
    if (strlen($content) <= 64) {
        $suffix = base64url_encode($content);
    } else {
        $suffix = base64url_encode(hash('sha512', $content, true));
    }

    return $prefix . $suffix;
}

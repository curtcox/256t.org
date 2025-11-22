// Check script for ECMAScript implementation
// This version uses Node.js APIs for CI testing but the core cid.js is browser-compatible

const fs = require('fs');
const path = require('path');

// Import the browser-compatible crypto polyfill for Node.js
const { webcrypto } = require('crypto');
global.crypto = webcrypto;
global.TextEncoder = TextEncoder;
global.fetch = require('node-fetch');

// Load the ECMAScript implementation
const { computeCid, downloadCid } = require('./cid.js');

const CIDS_DIR = path.resolve(__dirname, '..', '..', 'cids');
const BASE_URL = 'https://256t.org';

async function main() {
  const mismatches = [];
  const downloadFailures = [];
  let count = 0;
  
  const entries = fs.readdirSync(CIDS_DIR).sort();
  
  for (const entry of entries) {
    const fullPath = path.join(CIDS_DIR, entry);
    if (fs.statSync(fullPath).isDirectory()) {
      continue;
    }
    count += 1;
    const localContent = fs.readFileSync(fullPath);
    const expected = await computeCid(localContent);
    
    // Check local CID file
    if (expected !== entry) {
      mismatches.push([entry, expected]);
    }
    
    // Check downloaded content
    try {
      const { content, computed, isValid } = await downloadCid(BASE_URL, entry);
      if (!isValid) {
        downloadFailures.push([entry, computed]);
      } else if (!arraysEqual(content, localContent)) {
        downloadFailures.push([entry, 'content mismatch with local file']);
      }
    } catch (error) {
      downloadFailures.push([entry, error.message]);
    }
  }
  
  let hasErrors = false;
  
  if (mismatches.length > 0) {
    console.log('Found CID mismatches:');
    for (const [entry, expected] of mismatches) {
      console.log(`- ${entry} should be ${expected}`);
    }
    hasErrors = true;
  }
  
  if (downloadFailures.length > 0) {
    console.error('Found download validation failures:');
    for (const [cid, error] of downloadFailures) {
      console.error(`- ${cid}: ${error}`);
    }
    hasErrors = true;
  }
  
  if (hasErrors) {
    process.exit(1);
  }
  
  console.log(`All ${count} CID files match their contents.`);
  console.log(`All ${count} downloaded CIDs are valid.`);
}

function arraysEqual(a, b) {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});

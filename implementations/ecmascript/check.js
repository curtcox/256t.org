// Check script for ECMAScript implementation
// This version uses Node.js APIs for CI testing but the core cid.js is browser-compatible

const fs = require('fs');
const path = require('path');

// Import the browser-compatible crypto polyfill for Node.js
const { webcrypto } = require('crypto');
global.crypto = webcrypto;
global.TextEncoder = TextEncoder;

// Load the ECMAScript implementation
const { computeCid } = require('./cid.js');

const CIDS_DIR = path.resolve(__dirname, '..', '..', 'cids');

async function main() {
  let mismatches = 0;
  let count = 0;
  
  const entries = fs.readdirSync(CIDS_DIR);
  
  for (const entry of entries) {
    const fullPath = path.join(CIDS_DIR, entry);
    if (fs.statSync(fullPath).isDirectory()) {
      continue;
    }
    count += 1;
    const content = fs.readFileSync(fullPath);
    const expected = await computeCid(content);
    if (expected !== entry) {
      console.log(`${entry} should be ${expected}`);
      mismatches += 1;
    }
  }
  
  if (mismatches) {
    console.error(`Found ${mismatches} mismatched CID file(s).`);
    process.exit(1);
  }
  console.log(`All ${count} CID files match their contents.`);
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});

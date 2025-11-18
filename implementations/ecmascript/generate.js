// Generate script for ECMAScript implementation
// This version uses Node.js APIs for file generation

const fs = require('fs');
const path = require('path');

// Import the browser-compatible crypto polyfill for Node.js
const { webcrypto } = require('crypto');
global.crypto = webcrypto;
global.TextEncoder = TextEncoder;

// Load the ECMAScript implementation
const { computeCid } = require('./cid.js');

const EXAMPLES_DIR = path.resolve(__dirname, '..', '..', 'examples');
const CIDS_DIR = path.resolve(__dirname, '..', '..', 'cids');

async function main() {
  if (!fs.existsSync(CIDS_DIR)) {
    fs.mkdirSync(CIDS_DIR, { recursive: true });
  }
  
  const entries = fs.readdirSync(EXAMPLES_DIR);
  
  for (const entry of entries) {
    const fullPath = path.join(EXAMPLES_DIR, entry);
    if (fs.statSync(fullPath).isDirectory()) {
      continue;
    }
    const content = fs.readFileSync(fullPath);
    const cid = await computeCid(content);
    const cidPath = path.join(CIDS_DIR, cid);
    fs.writeFileSync(cidPath, content);
    console.log(`Generated ${cid}`);
  }
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});

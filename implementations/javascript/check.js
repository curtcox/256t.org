const fs = require('fs');
const path = require('path');
const { CIDS_DIR, computeCid } = require('./cid');

let mismatches = 0;
let count = 0;
for (const entry of fs.readdirSync(CIDS_DIR)) {
  const fullPath = path.join(CIDS_DIR, entry);
  if (fs.statSync(fullPath).isDirectory()) {
    continue;
  }
  count += 1;
  const content = fs.readFileSync(fullPath);
  const expected = computeCid(content);
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

import { readdirSync, readFileSync, statSync } from "node:fs";
import { CIDS_DIR, computeCid } from "./cid";

let mismatches = 0;
let count = 0;

for (const entry of readdirSync(CIDS_DIR)) {
  const fullPath = `${CIDS_DIR}/${entry}`;
  if (statSync(fullPath).isDirectory()) {
    continue;
  }
  count += 1;
  const content = readFileSync(fullPath);
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

import { CIDS_DIR, computeCid } from "./cid.ts";

let mismatches = 0;
let count = 0;

for await (const entry of Deno.readDir(CIDS_DIR)) {
  if (entry.isDirectory) {
    continue;
  }
  count += 1;
  const path = `${CIDS_DIR}/${entry.name}`;
  const content = await Deno.readFile(path);
  const expected = await computeCid(content);
  if (expected !== entry.name) {
    console.log(`${entry.name} should be ${expected}`);
    mismatches += 1;
  }
}

if (mismatches) {
  console.error(`Found ${mismatches} mismatched CID file(s).`);
  Deno.exit(1);
}

console.log(`All ${count} CID files match their contents.`);

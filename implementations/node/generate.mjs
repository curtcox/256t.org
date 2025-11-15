import { mkdirSync, readdirSync, readFileSync, writeFileSync, statSync } from 'node:fs';
import { EXAMPLES_DIR, CIDS_DIR, computeCid } from './cid.js';

mkdirSync(CIDS_DIR, { recursive: true });

for (const entry of readdirSync(EXAMPLES_DIR)) {
  const fullPath = `${EXAMPLES_DIR}/${entry}`;
  if (statSync(fullPath).isDirectory()) {
    continue;
  }
  const content = readFileSync(fullPath);
  const cid = computeCid(content);
  writeFileSync(`${CIDS_DIR}/${cid}`, content);
  console.log(`Wrote ${cid} from ${entry}`);
}

import { mkdirSync, readdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { CIDS_DIR, EXAMPLES_DIR, computeCid } from "./cid";

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

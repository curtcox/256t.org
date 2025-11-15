const fs = require('fs');
const path = require('path');
const { EXAMPLES_DIR, CIDS_DIR, computeCid } = require('./cid');

fs.mkdirSync(CIDS_DIR, { recursive: true });

for (const entry of fs.readdirSync(EXAMPLES_DIR)) {
  const fullPath = path.join(EXAMPLES_DIR, entry);
  if (fs.statSync(fullPath).isDirectory()) {
    continue;
  }
  const content = fs.readFileSync(fullPath);
  const cid = computeCid(content);
  fs.writeFileSync(path.join(CIDS_DIR, cid), content);
  console.log(`Wrote ${cid} from ${entry}`);
}

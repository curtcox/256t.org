const fs = require('fs');
const path = require('path');
const { CIDS_DIR, computeCid, downloadCid } = require('./cid');

const baseUrl = 'https://256t.org';

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
    const expected = computeCid(localContent);
    
    // Check local CID file
    if (expected !== entry) {
      mismatches.push([entry, expected]);
    }
    
    // Check downloaded content
    try {
      const { content, computed, isValid } = await downloadCid(baseUrl, entry);
      if (!isValid) {
        downloadFailures.push([entry, computed]);
      } else if (!content.equals(localContent)) {
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

main().catch((error) => {
  console.error('Unexpected error:', error);
  process.exit(1);
});

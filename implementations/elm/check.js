#!/usr/bin/env node
// CID checker for Elm implementation
// Verifies that all files in the cids/ directory have correct CIDs as filenames

import { computeCid, getCidFiles, readCidFile, CIDS_DIR } from './cid.js';

/**
 * Check if a CID file's name matches its content
 */
function checkCidFile(cid) {
  try {
    const content = readCidFile(cid);
    const expected = computeCid(content);
    return {
      cid,
      expected,
      matches: cid === expected,
    };
  } catch (error) {
    return {
      cid,
      error: error.message,
      matches: false,
    };
  }
}

/**
 * Main function to check all CID files
 */
function main() {
  const files = getCidFiles();
  const results = files.map(checkCidFile);

  const mismatches = results.filter((r) => !r.matches);
  const errors = results.filter((r) => r.error);

  let exitCode = 0;

  if (mismatches.length > 0) {
    console.log('Found CID mismatches:');
    mismatches.forEach((result) => {
      if (result.error) {
        console.log(`- ${result.cid}: Error - ${result.error}`);
      } else {
        console.log(`- ${result.cid} should be ${result.expected}`);
      }
    });
    exitCode = 1;
  }

  if (exitCode === 0) {
    console.log(`All ${files.length} CID files match their contents.`);
  }

  process.exit(exitCode);
}

main();

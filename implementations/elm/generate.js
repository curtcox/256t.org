#!/usr/bin/env node
// CID generator for Elm implementation
// Generates CIDs for all files in the examples/ directory

import { computeCid, EXAMPLES_DIR, CIDS_DIR } from './cid.js';
import { readFileSync, readdirSync, writeFileSync, mkdirSync } from 'node:fs';
import { join } from 'node:path';

/**
 * Generate CID for a file and write it to the cids/ directory
 */
function generateCid(filename) {
  const content = readFileSync(join(EXAMPLES_DIR, filename));
  const cid = computeCid(content);
  const outputPath = join(CIDS_DIR, cid);

  try {
    writeFileSync(outputPath, content);
    console.log(`${filename} -> ${cid}`);
    return { filename, cid, success: true };
  } catch (error) {
    console.error(`Error writing ${cid}: ${error.message}`);
    return { filename, cid, success: false, error: error.message };
  }
}

/**
 * Main function to generate CIDs for all example files
 */
function main() {
  // Ensure cids directory exists
  try {
    mkdirSync(CIDS_DIR, { recursive: true });
  } catch (error) {
    if (error.code !== 'EEXIST') {
      console.error(`Error creating cids directory: ${error.message}`);
      process.exit(1);
    }
  }

  const files = readdirSync(EXAMPLES_DIR).filter(
    (file) => !file.startsWith('.')
  );

  console.log(`Generating CIDs for ${files.length} files...`);
  const results = files.map(generateCid);

  const failures = results.filter((r) => !r.success);
  if (failures.length > 0) {
    console.error(`\nFailed to generate ${failures.length} CIDs`);
    process.exit(1);
  }

  console.log(`\nSuccessfully generated ${results.length} CIDs`);
}

main();

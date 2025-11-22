import { CIDS_DIR, computeCid, downloadCid } from "./cid.ts";

const baseUrl = "https://256t.org";

async function main() {
  const mismatches: Array<[string, string]> = [];
  const downloadFailures: Array<[string, string]> = [];
  let count = 0;

  const entries: Deno.DirEntry[] = [];
  for await (const entry of Deno.readDir(CIDS_DIR)) {
    if (!entry.isDirectory) {
      entries.push(entry);
    }
  }
  entries.sort((a, b) => a.name.localeCompare(b.name));

  for (const entry of entries) {
    count += 1;
    const path = `${CIDS_DIR}/${entry.name}`;
    const localContent = await Deno.readFile(path);
    const expected = await computeCid(localContent);

    // Check local CID file
    if (expected !== entry.name) {
      mismatches.push([entry.name, expected]);
    }

    // Check downloaded content
    try {
      const { content, computed, isValid } = await downloadCid(baseUrl, entry.name);
      if (!isValid) {
        downloadFailures.push([entry.name, computed]);
      } else if (!arraysEqual(content, localContent)) {
        downloadFailures.push([entry.name, "content mismatch with local file"]);
      }
    } catch (error) {
      downloadFailures.push([entry.name, error instanceof Error ? error.message : String(error)]);
    }
  }

  let hasErrors = false;

  if (mismatches.length > 0) {
    console.log("Found CID mismatches:");
    for (const [name, expected] of mismatches) {
      console.log(`- ${name} should be ${expected}`);
    }
    hasErrors = true;
  }

  if (downloadFailures.length > 0) {
    console.error("Found download validation failures:");
    for (const [cid, error] of downloadFailures) {
      console.error(`- ${cid}: ${error}`);
    }
    hasErrors = true;
  }

  if (hasErrors) {
    Deno.exit(1);
  }

  console.log(`All ${count} CID files match their contents.`);
  console.log(`All ${count} downloaded CIDs are valid.`);
}

function arraysEqual(a: Uint8Array, b: Uint8Array): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

main().catch((error) => {
  console.error("Unexpected error:", error);
  Deno.exit(1);
});

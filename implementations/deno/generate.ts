import { CIDS_DIR, EXAMPLES_DIR, computeCid } from "./cid.ts";

await Deno.mkdir(CIDS_DIR, { recursive: true });

for await (const entry of Deno.readDir(EXAMPLES_DIR)) {
  if (entry.isDirectory) {
    continue;
  }
  const path = `${EXAMPLES_DIR}/${entry.name}`;
  const content = await Deno.readFile(path);
  const cid = await computeCid(content);
  await Deno.writeFile(`${CIDS_DIR}/${cid}`, content);
  console.log(`Wrote ${cid} from ${entry.name}`);
}

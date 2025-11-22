import { createHash } from "node:crypto";
import { get } from "node:https";
import { fileURLToPath } from "node:url";
import { dirname, join, resolve } from "node:path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export const BASE_DIR = resolve(__dirname, "..", "..");
export const EXAMPLES_DIR = join(BASE_DIR, "examples");
export const CIDS_DIR = join(BASE_DIR, "cids");

const toBase64Url = (buffer: Buffer): string =>
  buffer
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");

export const encodeLength = (length: number): string => {
  const bytes = Buffer.alloc(6);
  bytes.writeUIntBE(length, 0, 6);
  return toBase64Url(bytes);
};

export const computeCid = (content: Buffer): string => {
  const prefix = encodeLength(content.length);
  const suffix =
    content.length <= 64
      ? toBase64Url(content)
      : toBase64Url(createHash("sha512").update(content).digest());
  return `${prefix}${suffix}`;
};

export interface DownloadResult {
  content: Buffer;
  computed: string;
  isValid: boolean;
}

export const downloadCid = (
  baseUrl: string,
  cid: string
): Promise<DownloadResult> => {
  return new Promise((resolve, reject) => {
    const url = `${baseUrl.replace(/\/$/, "")}/${cid}`;
    get(url, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode}: ${res.statusMessage}`));
        return;
      }
      const chunks: Buffer[] = [];
      res.on("data", (chunk) => chunks.push(chunk));
      res.on("end", () => {
        const content = Buffer.concat(chunks);
        const computed = computeCid(content);
        const isValid = computed === cid;
        resolve({ content, computed, isValid });
      });
    }).on("error", reject);
  });
};

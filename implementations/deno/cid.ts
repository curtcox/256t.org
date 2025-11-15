const toBase64Url = (bytes: Uint8Array): string => {
  let binary = "";
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/g, "");
};

export const BASE_DIR = new URL("../..", import.meta.url).pathname;
export const EXAMPLES_DIR = `${BASE_DIR}/examples`;
export const CIDS_DIR = `${BASE_DIR}/cids`;

export const encodeLength = (length: number): string => {
  const bytes = new Uint8Array(6);
  let value = length;
  for (let i = 5; i >= 0; i--) {
    bytes[i] = value & 0xff;
    value >>= 8;
  }
  return toBase64Url(bytes);
};

export const computeCid = async (content: Uint8Array): Promise<string> => {
  const prefix = encodeLength(content.byteLength);
  if (content.byteLength <= 64) {
    return `${prefix}${toBase64Url(content)}`;
  }
  const hashBuffer = await crypto.subtle.digest("SHA-512", content);
  return `${prefix}${toBase64Url(new Uint8Array(hashBuffer))}`;
};

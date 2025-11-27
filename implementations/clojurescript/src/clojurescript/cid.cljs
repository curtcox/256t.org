(ns clojurescript.cid
  (:require ["crypto" :as crypto]))

(defn- to-base64url
  "Convert buffer to Base64URL encoding (URL-safe, no padding)"
  [buffer]
  (.toString buffer "base64url"))

(defn encode-length
  "Encode length as 8-character Base64URL string (6 bytes)"
  [length]
  (let [buffer (js/Buffer.alloc 6)]
    (.writeUIntBE buffer length 0 6)
    (to-base64url buffer)))

(defn sha512-hash
  "Compute SHA-512 hash of content buffer"
  [content-buffer]
  (let [hasher (.createHash crypto "sha512")]
    (.update hasher content-buffer)
    (.digest hasher)))

(defn compute-cid
  "Compute CID from content (as bytes vector or Buffer).
   For content <= 64 bytes, uses the content itself (base64url encoded).
   For content > 64 bytes, uses SHA-512 hash (base64url encoded)."
  [content]
  (let [buffer (if (instance? js/Buffer content)
                 content
                 (js/Buffer.from (clj->js content)))
        prefix (encode-length (.-length buffer))]
    (str prefix
         (if (<= (.-length buffer) 64)
           (to-base64url buffer)
           (to-base64url (sha512-hash buffer))))))

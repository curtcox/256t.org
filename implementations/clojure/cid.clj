(ns implementations.clojure.cid
  (:import (java.security MessageDigest)
           (java.util Base64)
           (java.nio.file Paths)))

(def base-dir (.toAbsolutePath (Paths/get "" (into-array String []))))
(def examples-dir (.resolve base-dir "examples"))
(def cids-dir (.resolve base-dir "cids"))

(defn- to-base64url ^String [^bytes data]
  (-> (Base64/getUrlEncoder)
      (.withoutPadding)
      (.encodeToString data)))

(defn encode-length ^String [^long length]
  (let [bytes (byte-array 6)]
    (loop [idx 5
           remaining length]
      (when (>= idx 0)
        ;; `unchecked-byte` allows storing unsigned byte values (0-255)
        ;; without throwing an overflow exception when the high bit is set.
        (aset bytes idx (unchecked-byte (bit-and remaining 0xFF)))
        (recur (dec idx) (unsigned-bit-shift-right remaining 8))))
    (to-base64url bytes)))

(defn compute-cid ^String [^bytes content]
  (let [prefix (encode-length (alength content))]
    (str prefix
         (if (<= (alength content) 64)
           (to-base64url content)
           (let [digest (MessageDigest/getInstance "SHA-512")]
             (to-base64url (.digest digest content)))))))

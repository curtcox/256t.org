(ns clojurescript.check
  (:require [clojurescript.cid :refer [compute-cid]]
            ["fs" :as fs]
            ["path" :as path]))

;; Resolve cids directory relative to where the compiled JS runs from (project root)
(def cids-dir (path/resolve (js/process.cwd) "cids"))

(defn read-dir-sync
  "Read directory contents synchronously"
  [dir-path]
  (vec (fs/readdirSync dir-path)))

(defn read-file-sync
  "Read file contents as Buffer"
  [file-path]
  (fs/readFileSync file-path))

(defn is-file?
  "Check if path is a file"
  [full-path]
  (let [stats (fs/statSync full-path)]
    (.isFile stats)))

(defn check-cid-file
  "Check if a CID file's name matches its content"
  [filename]
  (let [full-path (path/join cids-dir filename)]
    (when (is-file? full-path)
      (let [content (read-file-sync full-path)
            expected (compute-cid content)]
        (when (not= expected filename)
          [filename expected])))))

(defn main
  "Main function to check all CID files"
  []
  (let [files (sort (read-dir-sync cids-dir))
        results (->> files
                     (map check-cid-file)
                     (remove nil?))
        file-count (count files)]
    (if (empty? results)
      (do
        (println (str "All " file-count " CID files match their contents."))
        0)
      (do
        (println "Found CID mismatches:")
        (doseq [[actual expected] results]
          (println (str "- " actual " should be " expected)))
        1))))

(defn -main
  "Entry point"
  [& _]
  (let [exit-code (main)]
    (js/process.exit exit-code)))

;; Run when executed as main script
(-main)

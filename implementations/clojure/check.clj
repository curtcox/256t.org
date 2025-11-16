(ns implementations.clojure.check
  (:require [implementations.clojure.cid :refer [cids-dir compute-cid]])
  (:import (java.nio.file Files)
           (java.io File)))

(defn regular-files []
  (->> (.listFiles (.toFile cids-dir))
       (filter #(.isFile ^File %))
       (sort-by #(.getName ^File %))))

(defn main []
  (let [files (regular-files)
        mismatches (->> files
                        (map (fn [^File f]
                               (let [content (Files/readAllBytes (.toPath f))
                                     expected (compute-cid content)
                                     actual (.getName f)]
                                 (when (not= expected actual)
                                   [actual expected]))))
                        (remove nil?))
        count (count files)]
    (if (empty? mismatches)
      (do
        (println (format "All %d CID files match their contents." count))
        0)
      (do
        (println "Found CID mismatches:")
        (doseq [[actual expected] mismatches]
          (println (format "- %s should be %s" actual expected)))
        1))))

(defn -main [& _]
  (System/exit (main)))

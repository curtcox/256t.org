#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(base64enc)
})

source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "cid.R"))

write_cid_file <- function(example_path) {
  content <- readBin(example_path, what = "raw", n = file.info(example_path)$size)
  cid <- compute_cid(content)
  destination <- file.path(CIDS_DIR, cid)
  writeBin(content, destination)
  cat(sprintf("Wrote %s from %s\n", basename(destination), basename(example_path)))
}

main <- function() {
  if (!dir.exists(CIDS_DIR)) {
    dir.create(CIDS_DIR)
  }

  examples <- list.files(EXAMPLES_DIR, full.names = TRUE)
  examples <- examples[!file.info(examples)$isdir]
  examples <- sort(examples)

  invisible(lapply(examples, write_cid_file))
}

main()

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(base64enc)
})

source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "cid.R"))

read_content <- function(path) {
  size <- file.info(path)$size
  readBin(path, what = "raw", n = size)
}

main <- function() {
  entries <- list.files(CIDS_DIR, full.names = TRUE)
  entries <- entries[!file.info(entries)$isdir]
  entries <- sort(entries)

  mismatches <- list()
  count <- 0

  for (entry in entries) {
    count <- count + 1
    actual <- basename(entry)
    content <- read_content(entry)
    expected <- compute_cid(content)
    if (!identical(actual, expected)) {
      mismatches[[length(mismatches) + 1]] <- list(actual = actual, expected = expected)
    }
  }

  if (length(mismatches) > 0) {
    cat("Found CID mismatches:\n")
    for (mismatch in mismatches) {
      cat(sprintf("- %s should be %s\n", mismatch$actual, mismatch$expected))
    }
    quit(status = 1)
  }

  cat(sprintf("All %d CID files match their contents.\n", count))
}

main()

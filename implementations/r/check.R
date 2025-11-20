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
  download_failures <- list()
  count <- 0
  base_url <- "https://256t.org"

  for (entry in entries) {
    count <- count + 1
    cid <- basename(entry)
    local_content <- read_content(entry)
    expected <- compute_cid(local_content)
    
    # Check local CID file
    if (!identical(cid, expected)) {
      mismatches[[length(mismatches) + 1]] <- list(cid = cid, expected = expected)
    }
    
    # Check downloaded content
    tryCatch({
      result <- download_cid(base_url, cid)
      if (!result$is_valid) {
        download_failures[[length(download_failures) + 1]] <- list(cid = cid, error = result$computed)
      } else if (!identical(result$content, local_content)) {
        download_failures[[length(download_failures) + 1]] <- list(cid = cid, error = "content mismatch with local file")
      }
    }, error = function(e) {
      download_failures[[length(download_failures) + 1]] <<- list(cid = cid, error = e$message)
    })
  }

  has_errors <- FALSE

  if (length(mismatches) > 0) {
    cat("Found CID mismatches:\n")
    for (mismatch in mismatches) {
      cat(sprintf("- %s should be %s\n", mismatch$cid, mismatch$expected))
    }
    has_errors <- TRUE
  }

  if (length(download_failures) > 0) {
    cat("Found download validation failures:\n", file = stderr())
    for (failure in download_failures) {
      cat(sprintf("- %s: %s\n", failure$cid, failure$error), file = stderr())
    }
    has_errors <- TRUE
  }

  if (has_errors) {
    quit(status = 1)
  }

  cat(sprintf("All %d CID files match their contents.\n", count))
  cat(sprintf("All %d downloaded CIDs are valid.\n", count))
}

main()
